# script to fill NaNs in fortress forest tower data using regression from other
# stations. the main purpose is to build a complete modelling data set that
# contains c('air_temp', 'rh', 'wind_speed', 'ppt', 'short_wave_in')
# short_wave_in will be handelled externally from this script and then passed back here

library(tidyverse)
library(plotly)
library(CRHMr)

max_gap_fill_linear <- 2
logfile <- 'logs/CRHMr_ffr_regress.log'

ffr_met <- readRDS('data/ffr_met_main_qaqc.rds') |> 
  select(
    datetime,
    AirTC_towerTop,
    AirTC_midTree,
    RH_towerTop,
    RH_midTree,
    US_WindSpeed_avg,
    US_WindSpeed_avg_dir,
    WindSpeed3cup_Avg,
    SnowDepth
  )

at_col_top <- 1
at_col_mid <- 2
rh_col_top <- 3
rh_col_mid <- 4
us_col <- 5
us_dir_col <- 6
cup_col <- 7
sd_col <- 8

pwl_met <- readRDS('data/pwl_met_server_at_rh_u_qaqc_filled.rds') 

pwl_rm_young_col <- 3
pwl_3cup_col <- 4

findGaps(ffr_met, gapfile = 'logs/ffr_gaps.csv', logfile = logfile)

# fill short gaps ----

# looking at these gaps we are ok to linear interpolate the air temperature data
# and RH since there are just a handfull of random 1 tx NANs but for the wind
# speed data we will need to gap fill from another station since the gaps are
# longer

data_gaps <- read_csv('logs/ffr_gaps.csv')
data_gaps

## air temperature top ----

ffr_at_fill_top <- CRHMr::interpolate(ffr_met, 
                                  varcols = at_col_top,
                                  methods = 'linear', 
                                  maxlength = max_gap_fill_linear)

still_have_gaps <- findGaps(ffr_at_fill_top, gapfile = 'logs/ffr_at_gaps.csv', quiet = F, logfile = logfile)

if(!still_have_gaps == F){
  warning("Air temp at tower top still has gaps after linear interpolation, will need to apply station regression fill.")
}

## air temperature mid ----

ffr_at_fill_mid <- CRHMr::interpolate(ffr_met, 
                                  varcols = at_col_mid,
                                  methods = 'linear', 
                                  maxlength = max_gap_fill_linear)

still_have_gaps <- findGaps(ffr_at_fill_mid, gapfile = 'logs/ffr_at_gaps.csv', quiet = F, logfile = logfile)

if(!still_have_gaps == F){
  warning("Air temp at tower mid still has gaps after linear interpolation, will need to apply station regression fill.")
}

## rh top ----

# We cannot regress or gap fill directly with relative humidity. The reason is
# that RH depends on both the absolute humidity and the temperature, so
# interpolating it will give bad values. The way to do it is 
# convert T + RH -> ea
# interpolate ea
# convert ea + T -> RH

ffr_at_rh_top <- ffr_met |> select(datetime,
                               t.1 = AirTC_towerTop,
                               rh.1 = RH_towerTop)

ffr_ea_top <- CRHMr::changeRHtoEa(obs = ffr_at_rh_top,
                              t.cols = 1,
                              rh.cols = 2,
                              logfile = logfile)

ffr_ea_fill_top <- CRHMr::interpolate(ffr_ea_top, 
                                  varcols = 2,
                                  methods = 'linear', 
                                  maxlength = max_gap_fill_linear)

## rh mid ----

ffr_at_rh_mid <- ffr_met |> select(datetime,
                                   t.1 = AirTC_midTree,
                                   rh.1 = RH_midTree)

ffr_ea_mid <- CRHMr::changeRHtoEa(obs = ffr_at_rh_mid,
                                  t.cols = 1,
                                  rh.cols = 2,
                                  logfile = logfile)

ffr_ea_fill_mid <- CRHMr::interpolate(ffr_ea_mid, 
                                      varcols = 2,
                                      methods = 'linear', 
                                      maxlength = max_gap_fill_linear)
## us wind speed ----

ffr_us_fill <- CRHMr::interpolate(ffr_met, 
                                      varcols = us_col,
                                      methods = 'linear', 
                                      maxlength = max_gap_fill_linear)

still_have_gaps <- findGaps(ffr_us_fill, gapfile = 'logs/ffr_us_gaps.csv', quiet = F, logfile = logfile)

if(!still_have_gaps == F){
  warning("Ultra sonic wind still has gaps after linear interpolation, will need to apply station regression fill.")
}

## 3cup wind speed ----

ffr_3cup_fill <- CRHMr::interpolate(ffr_met, 
                                  varcols = cup_col,
                                  methods = 'linear', 
                                  maxlength = max_gap_fill_linear)

still_have_gaps <- findGaps(ffr_3cup_fill, gapfile = 'logs/ffr_cup_gaps.csv', quiet = F, logfile = logfile)

if(!still_have_gaps == F){
  warning("3 cup wind still has gaps after linear interpolation, will need to apply station regression fill.")
}

## snow depth ----

ffr_sd_fill <- CRHMr::interpolate(ffr_met, 
                                    varcols = sd_col,
                                    methods = 'linear', 
                                    maxlength = 6*4) # fill 6 hours with linear

still_have_gaps <- findGaps(ffr_sd_fill, gapfile = 'logs/ffr_sd_gaps.csv', quiet = F, logfile = logfile)

if(!still_have_gaps == F){
  warning("snow depth still has gaps after linear interpolation, will need to apply station regression fill.")
}

# combine short gap filled into a df 

short_gaps_filled <- left_join(ffr_at_fill_top, ffr_at_fill_mid,) |> 
  left_join(ffr_us_fill) |> 
  left_join(ffr_3cup_fill) |> 
  left_join(ffr_sd_fill)

saveRDS(short_gaps_filled, 'data/ffr_t_u_sd_qaqc_shortfill.rds')

# fill long gaps ----

## wind speed ----

### build regressions to fill Ultra Sonic wind at FFR ----

ffr_wnd <- ffr_met |> 
  select(datetime, US_WindSpeed_avg, WindSpeed3cup_Avg)

# ffr US wind and ffr 3 cup which has the best association prior to 2022-10-28
# 13:15 the 3 cup at FFR had a bad calibration so need to separate this

ffr_3cup_cal_date <- as.POSIXct('2022-10-28 13:15', tz = 'Etc/GMT+6')

ffr_met_pre_3cup_cal <- ffr_wnd |> 
  filter(datetime < ffr_3cup_cal_date)

ffr_met_post_3cup_cal <- ffr_wnd |> 
  filter(datetime > ffr_3cup_cal_date)

# yikes, maybe dont use pre cal if we care about < 0.5 m.s winds
# ggplot(ffr_met_pre_3cup_cal, aes(WindSpeed3cup_Avg, US_WindSpeed_avg)) +
#   geom_point() +
#   geom_abline() +
#   geom_smooth(method = 'lm', formula=y~x) 

# ok looks great post cal
# ggplot(ffr_met_post_3cup_cal, aes(WindSpeed3cup_Avg, US_WindSpeed_avg)) +
#   geom_point() +
#   geom_abline() +
#   geom_smooth(method = 'lm', formula=y~x)

ffr_us_3cup_cal_regress_df <- regress(
  ffr_met_post_3cup_cal,
  primary.columns = 1,
  ffr_met_post_3cup_cal,
  secondary.columns = 2,
  plot = F,
  quiet = F,
  logfile = logfile
)

regress(
  ffr_met,
  primary.columns = us_col,
  pwl_met,
  secondary.columns = pwl_rm_young_col, 
  forceOrigin = F,
  plot = F,
  quiet = F,
  logfile = logfile
) 

regress(
  ffr_met,
  primary.columns = us_col,
  pwl_met,
  secondary.columns = pwl_3cup_col, 
  forceOrigin = F,
  plot = F,
  quiet = F,
  logfile = logfile
) 

# above the rm young has the better r2 and visual fit still fit origin here or else will not make sense
ffr_us_pwl_rm_young_regress_df <- regress(
  ffr_met,
  primary.columns = us_col,
  pwl_met,
  secondary.columns = pwl_rm_young_col, 
  forceOrigin = T,
  plot = F,
  quiet = F,
  logfile = logfile
)

### fill US wind gaps first round with best available i.e. FFR 3 cup with 0.98 R2 ----

ffr_us_fill <- CRHMr::impute(ffr_us_fill,
              primaryCols = 1,
              secondaryObs = ffr_3cup_fill,
              secondaryCols = 1,
              multipliers = ffr_us_3cup_cal_regress_df$slope,
              offsets = ffr_us_3cup_cal_regress_df$intercept,
              logfile = "logs/CRHMr_ffr_impute.log") |> 
  mutate(group = 'filled_us') |> 
  rename(wind_speed = US_WindSpeed_avg)

ffr_us_old <- ffr_met |> 
  select(datetime, wind_speed = US_WindSpeed_avg) |> 
  mutate(group = 'raw_us')

# rbind(ffr_us_fill, ffr_us_old) |>
#   ggplot(aes(datetime, wind_speed, colour = group)) +
#   geom_line()
# 
# ggplotly()

findGaps(ffr_us_fill, gapfile = 'logs/ffr_us_fill_gaps.csv', quiet = F, logfile = logfile)

## fill US wind gaps SECOND round with nect best available i.e. pwl rm young ----

ffr_us_fill <- CRHMr::impute(ffr_us_fill,
                             primaryCols = 1,
                             secondaryObs = pwl_met,
                             secondaryCols = pwl_rm_young_col,
                             multipliers = ffr_us_pwl_rm_young_regress_df$slope,
                             logfile = "logs/CRHMr_ffr_impute.log") |> 
  mutate(group = 'filled_us')

ffr_us_old <- ffr_met |> 
  select(datetime, wind_speed = US_WindSpeed_avg) |> 
  mutate(group = 'raw_us')

# rbind(ffr_us_fill, ffr_us_old) |> 
#   ggplot(aes(datetime, wind_speed, colour = group)) +
#   geom_line()
# 
# ggplotly()

stillhasgaps <- findGaps(ffr_us_fill, gapfile = 'logs/ffr_us_fill_gaps.csv', quiet = F, logfile = logfile)

stopifnot(!stillhasgaps)

saveRDS(ffr_us_fill, 'data/ffr_us_fill_gaps.rds')

### fill wind dir gaps with pwl (since dont have 3cup direction)
ffr_us_dir_fill<- ffr_met |> 
  select(datetime, US_WindSpeed_avg_dir) |> 
  left_join(pwl_met |> select(datetime, pwl_wind_dir = wind_dir)) |> 
  mutate(wind_dir = ifelse(is.na(US_WindSpeed_avg_dir), pwl_wind_dir, US_WindSpeed_avg_dir)) |> 
  select(datetime, wind_dir)

## air temperature ----

ffr_at_mid_col <- 2

ffr_at_top_col <- 1 

pwl_at_col <- 1

### build regressions with ffr air temp mid tower and top tower ----

# regress(
#   ffr_met,
#   primary.columns = ffr_at_mid_col,
#   ffr_met,
#   secondary.columns = ffr_at_top_col,
#   plot = T,
#   logfile = logfile
# ) 

ffr_at_mid_top_regress <- regress(
  ffr_met, 
  primary.columns = ffr_at_mid_col,
  ffr_met,
  secondary.columns = ffr_at_top_col,
  plot = F,
  logfile = logfile
)

### build regressions with ffr air temp mid tower and pwl at ----

# regress(
#   ffr_met, 
#   primary.columns = ffr_at_mid_col,
#   pwl_met,
#   secondary.columns = pwl_at_col,
#   plot = T,
#   logfile = logfile
# ) + geom_abline()

ffr_at_mid_pwl_regress <- regress(
  ffr_met, 
  primary.columns = ffr_at_mid_col,
  pwl_met,
  secondary.columns = pwl_at_col,
  plot = F,
  logfile = logfile
)

### use regressions to impute air temp where does not exist ----

# first from the mid tower at/rh sensor 
findGaps(ffr_met, gapfile = 'logs/ffr_at_gaps.csv', quiet = F, logfile = logfile)

ffr_at_fill <- CRHMr::impute(ffr_at_fill_mid,
                              primaryCols = 1,
                              secondaryObs = ffr_at_fill_mid,
                              secondaryCols = 1,
                              multipliers = ffr_at_mid_top_regress$slope,
                              offsets = ffr_at_mid_top_regress$intercept,
                              logfile = logfile) |> 
  mutate(group = 'filled') |> 
  rename(air_temp = AirTC_midTree)

ffr_met_old <- ffr_met |> 
  select(datetime, air_temp = AirTC_midTree) |> 
  mutate(group = 'raw')
# 
# rbind(ffr_at_fill, ffr_met_old) |>
#   ggplot(aes(datetime, air_temp, colour = group)) +
#   geom_line()
# 
# ggplotly()

# second from the PWL at/rh sensor 
findGaps(ffr_at_fill, gapfile = 'logs/ffr_at_gaps.csv', quiet = F, logfile = logfile)

ffr_at_fill <- CRHMr::impute(ffr_at_fill,
                              primaryCols = 1,
                              secondaryObs = pwl_met,
                              secondaryCols = pwl_at_col,
                              multipliers = ffr_at_mid_pwl_regress$slope,
                              offsets = ffr_at_mid_pwl_regress$intercept,
                              logfile = logfile) |> 
  mutate(group = 'filled') 

ffr_met_old <- ffr_met |> 
  select(datetime, air_temp = AirTC_midTree) |> 
  mutate(group = 'raw')

# rbind(ffr_at_fill, ffr_met_old) |>
#   ggplot(aes(datetime, air_temp, colour = group)) +
#   geom_line()
# 
# ggplotly()

still_have_gaps <- findGaps(ffr_at_fill, gapfile = 'logs/ffr_at_gaps.csv', quiet = F, logfile = logfile)

stopifnot(!still_have_gaps)

## relative humidity ----

# We cannot regress or gap fill directly with relative humidity. The reason is
# that RH depends on both the absolute humidity and the temperature, so
# interpolating it will give bad values. The way to do it is 
# convert T + RH -> ea
# interpolate ea
# convert ea + T -> RH

ffr_rh_mid_col <- 4

ffr_rh_top_col <- 3 

pwl_rh_col <- 2

### convert RH to EA for ffr and pwl ----

pwl_ea <- CRHMr::changeRHtoEa(obs = pwl_met,
                                  t.cols = 1,
                                  rh.cols = 2,
                                  logfile = logfile)

### build regressions with ffr Ea mid tower and top tower ----

# regress(
#   ffr_ea_mid, 
#   primary.columns = 2,
#   ffr_ea_top,
#   secondary.columns = 2,
#   plot = T,
#   logfile = logfile
# ) + geom_abline()

ffr_ea_mid_top_regress <- regress(
  ffr_ea_fill_mid, 
  primary.columns = 1,
  ffr_ea_fill_top,
  secondary.columns = 1,
  plot = F,
  logfile = logfile
)

### build regressions with ffr air temp mid tower and pwl at ----

# regress(
#   ffr_ea_mid, 
#   primary.columns = 2,
#   pwl_ea,
#   secondary.columns = 2,
#   plot = T,
#   logfile = logfile
# ) + geom_abline()

ffr_ea_pwl_regress <- regress(
  ffr_ea_fill_mid, 
  primary.columns = 1,
  pwl_ea,
  secondary.columns = 2,
  plot = F,
  logfile = logfile
)

### use regressions to impute Ea where does not exist ----

# first from the top tower at/rh sensor 
findGaps(ffr_ea_mid, gapfile = 'logs/ffr_ea_gaps.csv', quiet = F, logfile = logfile)
findGaps(pwl_ea, gapfile = 'logs/pwl_ea_gaps.csv', quiet = F, logfile = logfile)

# fill Ea using the top sensor
ffr_ea_fill <- CRHMr::impute(ffr_ea_fill_mid,
                              primaryCols = 1,
                              secondaryObs = ffr_ea_fill_top,
                              secondaryCols = 1,
                              multipliers = ffr_ea_mid_top_regress$slope,
                              offsets = ffr_ea_mid_top_regress$intercept,
                              logfile = logfile) |> 
  mutate(group = 'filled') 

ffr_ea_mid_old <- ffr_ea_mid |> 
  select(datetime, ea.1) |> 
  mutate(group = 'raw')

# rbind(ffr_ea_fill, ffr_ea_mid_old) |>
#   ggplot(aes(datetime, ea.1, colour = group)) +
#   geom_line()
# 
# ggplotly()

# second from the PWL at/rh sensor 
findGaps(ffr_ea_fill, gapfile = 'logs/ffr_ea_gaps.csv', quiet = F, logfile = logfile)

ffr_ea_fill <- CRHMr::impute(ffr_ea_fill,
                             primaryCols = 1,
                             secondaryObs = pwl_ea,
                             secondaryCols = 2,
                             multipliers = ffr_ea_pwl_regress$slope,
                             offsets = ffr_ea_pwl_regress$intercept,
                             logfile = logfile) |> 
  mutate(group = 'filled')

ffr_ea_mid_old <- ffr_ea_mid |> 
  select(datetime, ea.1) |> 
  mutate(group = 'raw')


rbind(ffr_ea_fill, ffr_ea_mid_old) |>
  ggplot(aes(datetime, ea.1, colour = group)) +
  geom_line()
# 
# ggplotly()

gaps <-
  findGaps(ffr_ea_fill,
           gapfile = 'logs/ffr_ea_gaps.csv',
           quiet = F,
           logfile = logfile)

# if errors out we still have gaps in th ea data ... 
stopifnot(!gaps)

### convert ea back to RH ---- 

ffr_at_rh <- ffr_at_fill |> 
  left_join(ffr_ea_fill) |> 
  select(-group) 

ffr_rh_fill <-
  CRHMr::changeEatoRH(
    obs = ffr_at_rh,
    t.cols = 1,
    ea.cols = 2,
    logfile = logfile) |> 
  rename(rh = rh.1)

# combine the filled obs ----

ffr_qc_fill_out <- ffr_rh_fill |> 
  left_join(ffr_us_fill) |> 
  left_join(ffr_us_dir_fill) |> 
  select(-group)

stillhasgaps <- findGaps(ffr_qc_fill_out, 
                         gapfile = 'logs/ffr_us_fill_gaps.csv',
                         quiet = F, 
                         logfile = logfile)

# write out obs file with c('air_temp', 'rh', 'wind_speed'), remaining vars will
# be handelled externally

saveRDS(ffr_qc_fill_out, 'data/ffr_t_rh_u_qaqc_fill.rds')

# bring in the shortwave in data from fortress ridge ---- 

frg <- readRDS('data/frg_qsi_qaqc_fill.rds') |> 
  rename(short_wave_in = short_in_Avg)

# bring in the pwl precip data corrected for undercatch ----

pc <- readRDS('data/pluvio-qaqc/pwl_pluvio_15_min_qaqc_undercatch_corr_ac.rds') |> 
  select(datetime, ppt)

# write out the final df of obs we need for CRHM modelling ---- 

ffr_qc_fill_out <- left_join(ffr_qc_fill_out, frg) |> 
  left_join(pc) |> 
  rename(t = air_temp,
         u = wind_speed, 
         Qsi = short_wave_in,
         p = ppt) |> 
  select(-wind_dir)

stillhasgaps <- findGaps(ffr_qc_fill_out, 
                         gapfile = 'logs/ffr_us_fill_gaps.csv',
                         quiet = F, 
                         logfile = logfile)

stopifnot(!stillhasgaps)

hasdups <- CRHMr::findDupes(ffr_qc_fill_out)

stopifnot(!hasdups)

saveRDS(ffr_qc_fill_out, 'data/ffr_crhm_modelling_obs.rds')

ffr_qc_after_midnight <- ffr_qc_fill_out |> slice(2:n())

CRHMr::writeObsFile(ffr_qc_after_midnight,
                    '../crhm-analysis/obs/ffr_crhm_modelling_obs.obs',
                    comment = 'This file consists of air temp, rh, and wind speed from the Waterloo Forest Tower (aka Fortress Forest Ride) see the R proj met-data-processing for qaqc and gap fill procedures. The radiation data is from Fortress Ridge and has been gap filled with Fortress Ridge South.')

ffr_qc_fill_out$t_ice_bulb <- psychRomet::ice_bulb_iter(ffr_qc_fill_out$t, ffr_qc_fill_out$rh/100)

saveRDS(ffr_qc_fill_out, 'data/ffr_crhm_obs_qaqc_gap_fill.rds')

# CRHMr::plotObs(ffr_qc_after_midnight, plotType = 'points')
# 
# ggsave('figs/crhmr_ffr_obs_facet.png')
