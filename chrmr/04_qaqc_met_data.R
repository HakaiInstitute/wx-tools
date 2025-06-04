# after combining all met data apply a rough qaqc to Forest Tower data.

library(tidyverse)
library(plotly)
library(CRHMr)

start_date <- as.POSIXct('2021-10-01 00:00', tz = 'Etc/GMT+6')
end_date <- as.POSIXct('2023-06-30 11:45', tz = 'Etc/GMT+6')
interval <- 60*15

complete_datetime <- data.frame(datetime = seq(start_date, end_date, by = interval))
calg_mag_declination <- 13.5 # for 2020
at_th <- 10 # deg C over 15 min
rh_th <- 50 # perc RH over 15 min
wind_th <- 4 # m/s over 15 min, chosen after close look at US_wind avg, US_max would need a different case probably
flatline_window <- 4*6 # hrs

# stdev filtering params
window_length <- 24*3*4 # 3 days 
lead_window <- list(1:window_length) # around 3 days to round out diurnal cycling
lag_window <- list(-1:-window_length)
window_length_narrow <- 24*1*4 # 3 days 
lead_window_narrow <- list(1:window_length_narrow) # around 3 days to round out diurnal cycling
lag_window_narrow <- list(-1:-window_length_narrow)
frac_records_required <- 0.9

options(ggplot2.discrete.colour= c(
  "#000000",
  "#E69F00",
  "#009E73",
  "#CC79A7",
  "#F0E442",
  "#56B4E9"
))

pwl <- readRDS('data/pwl_met_server_at_rh_u_qaqc_filled.rds')

storm_dates <- readRDS('../interception/data/select_storms_datetime.rds') |> 
  filter(use_in_storm_analysis == 'yes')

# bring in combined met station field downloads

waterloo_met <- readRDS('data/waterloo_1000_met_main.rds') |>
  select(
    datetime,
    AirTC_towerTop = AirTC_Avg,
    RH_towerTop = RH,
    SnowDepth
  ) 

# waterloo_ec <- readRDS('data/waterloo_1000_ec_main.rds') |> 
#   select(datetime, 
#          top_ec_wnd_spd = wnd_spd, 
#          top_ec_u_star = u_star)

irtc_wnd <- readRDS('data/1000x_wnd_irtc.rds') 

# define the units found in the logger program

unit_dict <- c(
  "BattV_Min" = "Volts",
  "US_WindSpeed_avg" = "m/s",
  "US_WindSpeed_avg_dir" = "deg. bad",
  "US_WindSpeed_avg_dir_std" = "deg.",
  "US_WindSpeed_max" = "m/s",         
  "US_WindSpeed_std" = "m/s",         
  "top_ec_wnd_spd" = "m/s",           
  "top_ec_u_star" = "m/s",            
  "NR_Wm2_Avg" = "W/m^2",               
  "NR_Wm2_2_Avg" = "W/m^2",                
  "RH_towerTop" = "%",              
  "low_ec_wnd_spd" = "m/s",         
  "low_ec_wnd_dir_compass" = "m/s",  
  "low_ec_u_star" = "m/s",              
  "RH_midTree" = "%",              
  "WindSpeed3cup_Avg" = "m/s",       
  "WindSpeed3cup_Max" = "m/s",       
  "WindSpeed3cup_Std" = "m/s",     
  "SnowDepth" = "m",             
  "IRTC_midTree" = "deg. C",            
  "IRTC_snow" = "deg. C",    
  "IRTC_trunk" = "deg. C",       
  "AirTC_towerTop" = "deg. C",      
  "AirTC_midTree" = "deg. C", 
  "tc_wetbulb_towerTop" = "deg. C",   
  "tc_wetbulb_towerMid" = "deg. C"      
)

# email form Warren Helgason regarding the different CSAT winds 
# 'Regarding the wind speed, you are correct. The Wind Vector command in
# Campbell dataloggers will give you the option to output the horizontal wind
# speed (simple mean of measured wind speeds) or the resultant of the vectors.
# See P 305 on the linked manual for the technical description:
# 'https://s.campbellsci.com/documents/af/manuals/cr3000.pdf

ec_lo <- readRDS('../eddy-cov/data/low-tower/low_tower_15min_2021_2023_qc_rough.rds') |> 
  select(
    datetime, 
    ec_lo_wind_speed = wind_speed,
    ec_lo_wind_dir_mag = wind_dir_mag
  ) |> 
  mutate(
    ec_lo_wind_dir_true = ec_lo_wind_dir_mag - calg_mag_declination) |> 
  select(datetime, ec_lo_wind_speed, ec_lo_wind_dir_true)

ec_hi <- readRDS('../eddy-cov/data/high-tower/ec_high_tower_30_min_2021_2023_qc_rough.rds')  |> 
  select(
    datetime, 
    ec_hi_wind_speed = wind_speed,
    ec_hi_wind_dir_mag = wind_dir_mag
  ) |> 
  mutate(
    ec_hi_wind_dir_true = ec_hi_wind_dir_mag - calg_mag_declination) |> 
  select(datetime, ec_hi_wind_speed, ec_hi_wind_dir_true)

main <- left_join(complete_datetime, waterloo_met, by = 'datetime') |> 
  left_join(irtc_wnd, by = 'datetime') |> 
  left_join(ec_lo, by = 'datetime') |> 
  left_join(ec_hi, by = 'datetime') |> 
  select(-starts_with('RECORD'), -c('USWindSpeed_Avg', 'USWindDirection_Std', 'USWindDirection_Avg')) |> 
  rename(US_WindSpeed_avg = USWindSpeed_S_WVT, 
         US_WindSpeed_avg_dir = USWindDir_D1_WVT, 
         US_WindSpeed_avg_dir_std = USWindDir_SD1_WVT, 
         US_WindSpeed_max = USWindSpeed_Max, 
         US_WindSpeed_std = USWindSpeed_Std) |> 
  filter(datetime >= start_date) 

saveRDS(main, 'data/ffr_met_main.rds')
write.csv(main, 'data/ffr_met_main.csv')

ffr_met_main <- readRDS('data/ffr_met_main.rds') |> 
  select(-starts_with('ec_lo'), -starts_with('ec_hi'))

# define filters
glob_hi <- 9999
glob_lo <- -9999
irtc_hi <- 200

# blanket filters across all columns 
df_long <- pivot_longer(ffr_met_main, -datetime) |> 
  filter(value <= glob_hi,
         value >= glob_lo)

# assign units to each var
# df_long$units <- unit_dict[df_long$name]

## column specific filters ####

## snow depth ####

# apply offset before was applied to logger

# df_long |> 
#   filter(name %in% c('SnowDepth')) |> 
#   ggplot(aes(x = datetime, y = value)) +
#   geom_line() 

df_long$value[df_long$name == 'SnowDepth' & 
               df_long$datetime < as.POSIXct('2022-03-04 17:15:00', tz = 'Etc/GMT+6')] <- 
  df_long$value[df_long$name == 'SnowDepth' & 
                  df_long$datetime < as.POSIXct('2022-03-04 17:15:00', tz = 'Etc/GMT+6')] + (1.66 - 0.142)

# final offset off of measured sd during winter... not applied yet due to scatter need to check sr50 height off ground in write in rain book

# 2022-11-15 11:00:00 measured 61 cm and sr50 shows 70
# 2022-11-30 11:00:00 measured 76 and sr50 74
# 
# on feb 15 2023 measured 99 cm snow depth and sr50 read 112 cm
# 2022-04-06 10:00:00 measured 171 sr50 read 175 

# df_long |> 
#   filter(name %in% c('SnowDepth')) |> 
#   ggplot(aes(x = datetime, y = value)) +
#   geom_line() 

# time range flag

df_long$value[df_long$name == 'SnowDepth' & 
               df_long$datetime >= as.POSIXct('2022-03-01 19:15', tz = 'Etc/GMT+6') & 
               df_long$datetime <= as.POSIXct('2022-03-02 01:00', tz = 'Etc/GMT+6')] <- NA

df_long$value[df_long$name == 'SnowDepth' & 
               df_long$datetime >= as.POSIXct('2022-03-02 20:00', tz = 'Etc/GMT+6') & 
               df_long$datetime <= as.POSIXct('2022-03-03 02:00', tz = 'Etc/GMT+6')] <- NA

df_long$value[df_long$name == 'SnowDepth' & 
               df_long$datetime == as.POSIXct('2022-03-01 07:00', tz = 'Etc/GMT+6')] <- NA

# spike detect

sd_wide <- df_long |> 
  pivot_wider() |> 
  select(datetime, SnowDepth) |> as.data.frame()

# ggplot(sd_wide|> 
#          filter(is.na(SnowDepth) == F), aes(datetime, SnowDepth)) + geom_line()

max_iter <- 0

# sd_wide is returned as the despiked df
while (max_iter < 1000) {

  cur_df <- sd_wide |> 
    filter(is.na(SnowDepth) == F) |> 
    CRHMr::deleteSpikes(
      colnum = 1,
      threshold = 0.30,
      spike_direction = 'hi'
    )
  
  if(all(cur_df==0)){
    sd_wide <- sd_wide
    break
    }
  
  sd_wide <- cur_df
  
  max_iter <- max_iter + 1

}

#### detect flatlines ---- 

# appears that when snow is covering the T/RH sensor we get a very clear 0 deg.
# C flatline (solar also 0red during this time)

sd_flatline_dates <- sd_wide |> 
  filter(is.na(SnowDepth) == F) |> 
  CRHMr::findFlatLines(1, window_size = flatline_window)

# above returns 0 if no flatlines
if(all(sd_flatline_dates == 0)){
  # i.e. there are no spikes
  sd_flatline_del <- sd_wide
} else {
  # CRHMr::plotFlags(at_spike_delete, at_flatline_dates, at_col)
  # plotly::ggplotly()
  
  sd_flatline_del <- sd_spike_delete |> 
    filter(is.na(SnowDepth) == F) |> 
    CRHMr::deleteFlatLines(1, window_size = flatline_window)
}

### STDEV check on rolling window WIDE ----

sd_spike_delete_nonan <- sd_flatline_del |>
  filter(is.na(SnowDepth) == F)

sd_spike_dates <- CRHMr::findSpikesStdevWindow(sd_spike_delete_nonan,
                                               min_frac_records = frac_records_required,
                                               colnum = 1,
                                               lead_window = lead_window,
                                               lag_window = lag_window,
                                               number_sd = 20,
                                               include_start_end = F
)

# above returns 0 if no spikes
if(all(sd_spike_dates == 0)){
  # i.e. there are no spikes
  sd_spike_delete <- sd_spike_delete_nonan
} else {
  
  CRHMr::plotFlags(sd_spike_delete_nonan, sd_spike_dates, 1)

  # plotly::ggplotly()
  
  sd_spike_delete <- sd_spike_delete_nonan |> 
    filter(is.na(SnowDepth) == F) |> 
    CRHMr::deleteSpikesStdevWindow(min_frac_records = frac_records_required,
                                   colnum = 1,
                                   lead_window = lead_window,
                                   lag_window = lag_window,
                                   number_sd = 20,
                                   include_start_end = F
    )
}

### STDEV check on rolling window NARROW ----

sd_spike_delete_nonan <- sd_spike_delete |>
  filter(is.na(SnowDepth) == F)

sd_spike_dates <- CRHMr::findSpikesStdevWindow(sd_spike_delete_nonan,
                                               min_frac_records = frac_records_required,
                                               colnum = 1,
                                               lead_window = lead_window_narrow,
                                               lag_window = lag_window_narrow,
                                               number_sd = 20,
                                               include_start_end = F
)

# above returns 0 if no spikes
if(all(sd_spike_dates == 0)){
  # i.e. there are no spikes
  sd_spike_delete <- sd_spike_delete_nonan
} else {
  
  CRHMr::plotFlags(sd_spike_delete_nonan, sd_spike_dates, 1)
  
  plotly::ggplotly()
  
  sd_spike_delete <- sd_spike_delete_nonan |> 
    filter(is.na(SnowDepth) == F) |> 
    CRHMr::deleteSpikesStdevWindow(min_frac_records = frac_records_required,
                                   colnum = 1,
                                   lead_window = lead_window_narrow,
                                   lag_window = lag_window_narrow,
                                   number_sd = 20,
                                   include_start_end = F
    )
}

df_long <- df_long |> 
  filter(!name %in% 'SnowDepth') |> 
  rbind(sd_spike_delete |> pivot_longer(!datetime)) 

## IRTC ####

irtc_names <- c('IRTC_midTree', 'IRTC_snow', 'IRTC_trunk')

df_irtc <- df_long |> 
  filter(name %in% irtc_names) |> 
  group_by(name) |> 
  mutate(
      med = zoo::rollapply(value, width = 96, FUN = median, na.rm = T, align = 'center', partial = T),
      sd = zoo::rollapply(value, width = 96, FUN = sd, na.rm = T, align = 'center', partial = T),
      stdep = (value - med) / sd,
      flag = ifelse(stdep > 5, T, F),
      flag = ifelse(value > irtc_hi, T, flag))

# df_irtc |>
#   ggplot(aes(x = datetime, y = value, group = name, colour = flag)) +
#   geom_point() +
#   facet_wrap(~name)
# 
# ggplotly()

# confirm remove flagged values

df_irtc_fltr <- df_irtc |> 
  mutate(value =
    case_when(flag == F ~ value,
              TRUE ~ NA)
  ) |> 
  select(-c(med, sd, stdep, flag))

df_long <- df_long |> 
  filter(!name %in% irtc_names) |> 
  rbind(df_irtc_fltr) 

# df_irtc_fltr |> 
#   ggplot(aes(x = datetime, y = value, group = name, colour = name)) +
#   geom_point() 

## Air Temp ####

# df |> 
#   ggplot(aes(x = AirTC_midTree, y = AirTC_towerTop)) +
#   geom_point() 

# ggplotly()

# look at time series

# df_long |> 
#   filter(name %in% c('AirTC_midTree', 'AirTC_towerTop')) |> 
#   ggplot(aes(x = datetime, y = value, group = name, colour = name)) +
#   geom_point() 

# ggplotly()

# T + RH sensor seems erroneous prior to replacement on Mar 8, 2022

df_long$value[df_long$name == 'AirTC_towerTop' & 
               df_long$datetime < as.POSIXct('2022-03-08 17:00', tz = 'Etc/GMT+6')] <- NA

# flag erroneous temp values mid sensor 

df_long$value[df_long$name == 'AirTC_midTree' & 
                df_long$datetime > as.POSIXct('2022-03-22 15:15', tz = 'Etc/GMT+6') & 
                df_long$datetime < as.POSIXct('2022-03-22 16:45', tz = 'Etc/GMT+6')] <- NA


# df_long |>
#   filter(name %in% c('AirTC_midTree', 'AirTC_towerTop')) |>
#   ggplot(aes(x = datetime, y = value, group = name, colour = name)) +
#   geom_line()
# 
# ggplotly()

# spike detection

at_top_no_nan <- df_long |> 
  filter(name %in% c('AirTC_towerTop'),
         is.na(value) == F) |> 
  pivot_wider() |> 
  as.data.frame()

at_spikes <- CRHMr::findSpikes(at_top_no_nan,
                    colnum = 1, 
                    threshold = at_th,
                    spike_direction = 'both', 
                    logfile = 'logs/CRHMr_ffr_spikes.log')

stopifnot(at_spikes == 0)

at_mid_no_nan <- df_long |> 
  filter(name %in% c('AirTC_midTree'),
         is.na(value) == F) |> 
  pivot_wider()|> 
  as.data.frame()

at_spikes <- CRHMr::findSpikes(at_mid_no_nan,
                               colnum = 1, 
                               threshold = at_th,
                               spike_direction = 'both', 
                               logfile = 'logs/CRHMr_ffr_spikes.log')

# if errors out here or above need to initiate spike removal for air temp 
stopifnot(at_spikes == 0)

# flatline check

flatlines <- findFlatLines(at_top_no_nan, 1, window_size = 4, 
                           logfile = 'logs/CRHMr_ffr_flats.log')

flatlines <- findFlatLines(at_mid_no_nan, 1, window_size = 4, 
                           logfile = 'logs/CRHMr_ffr_flats.log')

# df_at_fltr <- df_at |> 
#   mutate(value =
#            case_when(flag == F ~ value)
#   ) |> 
#   select(-c(med, sd, stdep, err))

# check temp against pwl data 

# pwl |>
#   pivot_longer(!datetime) |>
#   filter(name == 'air_temp') |>
#   rbind(df_at_fltr |> select(datetime, name, value)) |>
#   ggplot(aes(x = datetime, y = value, colour = name)) +
#   geom_line()
# 
# ggplotly()


## RH ####

# df |> 
#   ggplot(aes(x = RH_midTree, y = RH_towerTop)) +
#   geom_point() 
# 
# # ggplotly()
# 
# df_long |>
#   filter(name %in% c('RH_midTree', 'RH_towerTop')) |>
#   ggplot(aes(x = datetime, y = value, group = name, colour = name)) +
#   geom_line()
# 
# ggplotly()

# forest tower program does not have if > 100 set to 100 so we add this in here

df_long$value[df_long$name == 'RH_midTree' & 
                df_long$value > 100] <- 100

df_long$value[df_long$name == 'RH_towerTop' & 
                df_long$value > 100] <- 100

# some problem points right when the sensor was installed so we set to NA

df_long$value[df_long$name == 'RH_towerTop' & 
               df_long$datetime < as.POSIXct('2022-03-08 17:00', tz = 'Etc/GMT+6')] <- NA

df_long$value[df_long$name == 'RH_midTree' & 
                df_long$datetime > as.POSIXct('2022-03-22 15:15', tz = 'Etc/GMT+6') & 
                df_long$datetime <= as.POSIXct('2022-03-22 16:30', tz = 'Etc/GMT+6')] <- NA

# simple spike detect (low threshold)

df_long$value[df_long$name == 'RH_towerTop' & df_long$value < 5] <- NA

# spike detection

rh_top_no_nan <- df_long |> 
  filter(name %in% c('RH_towerTop'),
         is.na(value) == F) |> 
  pivot_wider() |> 
  as.data.frame()

rh_spikes <- CRHMr::findSpikes(rh_top_no_nan,
                               colnum = 1, 
                               threshold = rh_th,
                               spike_direction = 'both', 
                               logfile = 'logs/CRHMr_ffr_spikes.log')

stopifnot(rh_spikes == 0)

rh_mid_no_nan <- df_long |> 
  filter(name %in% c('RH_midTree'),
         is.na(value) == F) |> 
  pivot_wider()|> 
  as.data.frame()

rh_spikes <- CRHMr::findSpikes(rh_mid_no_nan,
                               colnum = 1, 
                               threshold = rh_th,
                               spike_direction = 'both', 
                               logfile = 'logs/CRHMr_ffr_spikes.log')

# if errors out here or above need to initiate spike removal for air temp 
stopifnot(rh_spikes == 0)

# flatline check

# these all checkout okay no need to remove just a foggy day. same obs on pwl rh

flatlines <- findFlatLines(rh_top_no_nan, 1, window_size = 4, 
                           logfile = 'logs/CRHMr_ffr_flats.log')

flatlines <- findFlatLines(rh_mid_no_nan, 1, window_size = 4, 
                           logfile = 'logs/CRHMr_ffr_flats.log')

# df_long |> 
#   filter(name %in% c('RH_midTree', 'RH_towerTop')) |> 
#   ggplot(aes(x = datetime, y = value, group = name, colour = name)) +
#   geom_point() 
# 
# pwl |> 
#   select(datetime, name, value) |> 
#   filter(name == 'Relative.Humidity') |> 
#   rbind(df_long |> 
#           select(datetime, name, value) |> 
#           filter(name %in%  c('RH_midTree', 'RH_towerTop'))) |> 
#   ggplot(aes(x = datetime, y = value, colour = name)) +
#   geom_point() 
# 
# ggplotly()

## ice bulb temperature ####

# add in corrected at and rh to df_long for T_i calc

# clean df for calculation

df_at_rh <- df_long |>
  pivot_wider(id_cols = c(datetime), names_from = name, values_from = value) |>
  select(datetime, AirTC_towerTop, AirTC_midTree, RH_towerTop, RH_midTree)

# calculate ice bulb, takes an minute or so, save to file to avoid reprocessing

Sys.time()

df_at_rh$tc_wetbulb_towerTop <- psychRomet::ice_bulb_iter(df_at_rh$AirTC_towerTop, df_at_rh$RH_towerTop/100)

Sys.time()

df_at_rh$tc_wetbulb_towerMid <- psychRomet::ice_bulb_iter(df_at_rh$AirTC_midTree, df_at_rh$RH_midTree/100)

Sys.time()

df_ti <- df_at_rh |> select(-c(AirTC_towerTop, AirTC_midTree, RH_towerTop, RH_midTree))

Sys.time()

# df_ti |> 
#   ggplot(aes(x = tc_wetbulb_towerTop, y = tc_wetbulb_towerMid)) +
#   geom_point() +
#   geom_abline(slope=1, intercept=0) 
# 
# ggplotly()
# 
# # look at time series
# 
# df_long |>
#   filter(name %in% c('tc_wetbulb_towerTop', 'tc_wetbulb_towerMid')) |>
#   ggplot(aes(x = datetime, y = value, group = name, colour = name)) +
#   geom_point()

# df to long for rbind to main df_long 

df_ti_long <- df_ti |>
  pivot_longer(tc_wetbulb_towerTop:tc_wetbulb_towerMid)

df_ti_long <-  df_ti_long|> 
  mutate(flag = F) |> 
  left_join(storm_dates) |> 
  select(names(df_long))

df_long <- rbind(df_long, df_ti_long) 

# df_long |>  
#      filter(datetime == as.POSIXct('2022-03-18 12:30', tz = 'Etc/GMT+6')) |> 
#      View()

## Wind Speed ####

ffr_sel_wind <- c("US_WindSpeed_avg", "low_ec_wnd_spd", "top_ec_wnd_spd", 'WindSpeed3cup_Avg')

pwl_sel_wind <- c('WindSpeed_S_WVT', 'PluvioWind_Avg')

# look at saved html under the figs folder 
# df_long |> 
#   filter(name %in% ffr_sel_wind) |> 
#   select(datetime, name, value) |> 
#   rbind(pwl |> pivot_longer(!datetime) |> filter(name %in% pwl_sel_wind)) |> 
#   ggplot(aes(datetime, value, colour = name)) + 
#   geom_line()
# 
# ggplotly()

# spike detection 

us_wnd_col_num <- 1 # CRMR doesnt count datetime as a col 

us_wnd_no_nan <- df_long |> 
  pivot_wider() |> 
  filter(is.na(US_WindSpeed_avg) == F) |> 
  as.data.frame() |> 
  select(datetime, starts_with('US_'))

us_wnd_spikes <-
  CRHMr::findSpikes(us_wnd_no_nan,
                    colnum = us_wnd_col_num, 
                    threshold = wind_th,
                    spike_direction = 'hi', 
                    logfile = 'logs/CRHMr_ffr_spikes.log')

sub_us_wnd <- df_long |> 
  filter(name == 'US_WindSpeed_avg') |> 
  filter(datetime %in% us_wnd_spikes)

df_long |>
  filter(name %in% ffr_sel_wind) |>
  select(datetime, name, value) |>
  rbind(pwl |> pivot_longer(!datetime) |> filter(name %in% pwl_sel_wind)) |>
  ggplot(aes(datetime, value)) +
  geom_line(aes(colour = name)) +
  geom_point(data = sub_us_wnd, aes(x = datetime))

# ggplotly()

spike <- T

counter <- 0

# repeat the despike function until we remove all obs greater than the threshold
while(TRUE %in% spike){
  counter <- counter + 1
  
  us_wnd_no_nan <- deleteSpikes(us_wnd_no_nan,
                                 colnum = us_wnd_col_num, 
                                 threshold = wind_th,
                                 spike_direction = 'hi', 
                                logfile = 'logs/CRHMr_ffr_spikes.log'
                                )
  
  us_wnd_no_nan <- us_wnd_no_nan |> 
    filter(is.na(US_WindSpeed_avg) == F)
  
  diff_wind <- diff(us_wnd_no_nan$US_WindSpeed_avg)
  
  spike <- diff_wind > wind_th
  
  sum(spike)
  
  if(counter >= 1000){
    paste('reached 1000 iterations, stopping early.')
    
    break
  }
}

# ggplot(us_wnd_no_nan, aes(datetime, US_WindSpeed_avg)) +
#   geom_line()

# ggplotly()

# look for flatlines 

flatlines <- findFlatLines(us_wnd_no_nan, us_wnd_col_num, window_size = 4, 
                           logfile = 'logs/CRHMr_ffr_flats.log')

sub_us_wnd_fltlns <- us_wnd_no_nan |> 
  filter(datetime %in% flatlines)

# ggplot(us_wnd_no_nan, aes(datetime, US_WindSpeed_avg)) +
#   geom_line() +
#   geom_point(data = sub_us_wnd_fltlns, aes(x = datetime), shape = 4, colour = 'red')

# ggplotly()

# also useful to compare against other wind insts. to check 

# df_long |> 
#   filter(name %in% ffr_sel_wind) |> 
#   select(datetime, name, value) |> 
#   rbind(pwl |> pivot_longer(!datetime) |> filter(name %in% pwl_sel_wind)) |> 
#   ggplot(aes(datetime, value)) + 
#   geom_line(aes(colour = name)) +
#   geom_point(data = sub_us_wnd_fltlns, aes(x = datetime), shape = 4, colour = 'red')
# 
# ggplotly()

# delete the flatlines

us_wnd_no_nan_no_flat <- deleteFlatLines(us_wnd_no_nan, us_wnd_col_num, window_size = 4, 
                                         logfile = 'logs/CRHMr_ffr_flats.log')

ggplot(us_wnd_no_nan_no_flat, aes(datetime, US_WindSpeed_avg)) +
  geom_line() +
  geom_point() +
  geom_point(data = sub_us_wnd_fltlns, aes(x = datetime), shape = 4, colour = 'red')

# ggplotly()

# still missing some problem points after the global wind filter, only qaqc'd additional time periods for the select 12 storms

# convert the ultra sonic wind df to long to apply same treatment to all vars
# assuming here that if the avg wind speed is err'd than the max and other vars
# are too

us_wnd_long <- us_wnd_no_nan_no_flat |> 
  pivot_longer(!datetime)

# basic hi threshold, also set avg US to NA where max is NAN'd

# us_wnd_long$value[us_wnd_long$name == 'US_WindSpeed_avg' & us_wnd_long$value > 40] <- NA
# 
# us_wnd_long$value[us_wnd_long$name == 'US_WindSpeed_avg' & us_wnd_long$value < 0] <- NA
# 
# us_wnd_long$value[us_wnd_long$name == 'US_WindSpeed_max' & us_wnd_long$value > 40] <- NA
# 
# us_wnd_long$value[us_wnd_long$name == 'US_WindSpeed_max' & us_wnd_long$value < 0] <- NA

# manual wind qaqc 2022 wat yr ----

# powerline 3 cup snow covered wind speed

# todo$value[us_wnd_long$datetime == as.POSIXct('2021-11-14 19:30', tz = 'Etc/GMT+6') & 
#                     us_wnd_long$datetime <= as.POSIXct('2021-11-15 12:45', tz = 'Etc/GMT+6')] <- NA
# todo$value[us_wnd_long$datetime == as.POSIXct('2021-11-28 18:45', tz = 'Etc/GMT+6') & 
#              us_wnd_long$datetime <= as.POSIXct('2021-11-30 06:00', tz = 'Etc/GMT+6')] <- NA

# snow covered US sensor, generally can tell as the US sensor is < 3 cup pwl when covered with snow ----

us_wnd_long$value[us_wnd_long$datetime > as.POSIXct('2021-10-29 14:45', tz = 'Etc/GMT+6') &
                    us_wnd_long$datetime < as.POSIXct('2021-10-31 04:45', tz = 'Etc/GMT+6')] <- NA
                    
us_wnd_long$value[us_wnd_long$datetime == as.POSIXct('2021-11-08 05:30', tz = 'Etc/GMT+6')] <- NA

us_wnd_long$value[us_wnd_long$datetime >= as.POSIXct('2021-11-14 10:00', tz = 'Etc/GMT+6') & 
                    us_wnd_long$datetime <= as.POSIXct('2021-11-20 07:15', tz = 'Etc/GMT+6')] <- NA

us_wnd_long$value[us_wnd_long$datetime >= as.POSIXct('2021-11-27 19:00', tz = 'Etc/GMT+6') & 
                    us_wnd_long$datetime <= as.POSIXct('2021-11-30 06:00', tz = 'Etc/GMT+6')] <- NA

us_wnd_long$value[us_wnd_long$datetime >= as.POSIXct('2021-12-01 20:00', tz = 'Etc/GMT+6') & 
                    us_wnd_long$datetime <= as.POSIXct('2021-12-03 19:30', tz = 'Etc/GMT+6')] <- NA

us_wnd_long$value[us_wnd_long$datetime >= as.POSIXct('2021-12-05 10:00', tz = 'Etc/GMT+6') & 
                    us_wnd_long$datetime <= as.POSIXct('2021-12-05 18:30', tz = 'Etc/GMT+6')] <- NA

us_wnd_long$value[us_wnd_long$datetime >= as.POSIXct('2021-12-18 14:00', tz = 'Etc/GMT+6') & 
                    us_wnd_long$datetime <= as.POSIXct('2021-12-20 13:00', tz = 'Etc/GMT+6')] <- NA

us_wnd_long$value[us_wnd_long$datetime >= as.POSIXct('2021-12-22 22:00', tz = 'Etc/GMT+6') & 
                    us_wnd_long$datetime <= as.POSIXct('2021-12-27 14:30', tz = 'Etc/GMT+6')] <- NA

us_wnd_long$value[us_wnd_long$datetime >= as.POSIXct('2022-01-02 05:15', tz = 'Etc/GMT+6') & 
                    us_wnd_long$datetime <= as.POSIXct('2022-01-08 15:00', tz = 'Etc/GMT+6')] <- NA

## after this the reconyx was installed and have better idea of US wind QAQC 

us_wnd_long$value[us_wnd_long$datetime >= as.POSIXct('2022-01-18 06:15', tz = 'Etc/GMT+6') & 
                    us_wnd_long$datetime <= as.POSIXct('2022-01-18 18:16', tz = 'Etc/GMT+6')] <- NA

us_wnd_long$value[us_wnd_long$datetime >= as.POSIXct('2022-01-30 15:15', tz = 'Etc/GMT+6') & 
                    us_wnd_long$datetime <= as.POSIXct('2022-01-31 09:30', tz = 'Etc/GMT+6')] <- NA


# feb 19, on feb 14 16:00 hr US wind switches to weird diurnal tempurate fluctuation. fixes its self on mar 4 14:15 hr.

us_wnd_long$value[us_wnd_long$datetime >= as.POSIXct('2022-02-14 18:00', tz = 'Etc/GMT+6') & 
                us_wnd_long$datetime <= as.POSIXct('2022-02-16 12:00', tz = 'Etc/GMT+6')] <- NA

us_wnd_long$value[us_wnd_long$datetime >= as.POSIXct('2022-02-19 07:45', tz = 'Etc/GMT+6') & 
                    us_wnd_long$datetime <= as.POSIXct('2022-03-04 10:30', tz = 'Etc/GMT+6')] <- NA

us_wnd_long$value[us_wnd_long$datetime >= as.POSIXct('2022-03-06 02:00', tz = 'Etc/GMT+6') & 
                    us_wnd_long$datetime <= as.POSIXct('2022-03-06 11:00', tz = 'Etc/GMT+6')] <- NA

us_wnd_long$value[us_wnd_long$datetime >= as.POSIXct('2022-03-07 02:00', tz = 'Etc/GMT+6') & 
                    us_wnd_long$datetime <= as.POSIXct('2022-03-07 11:00', tz = 'Etc/GMT+6')] <- NA

us_wnd_long$value[us_wnd_long$datetime >= as.POSIXct('2022-03-07 02:00', tz = 'Etc/GMT+6') & 
                    us_wnd_long$datetime <= as.POSIXct('2022-03-07 11:00', tz = 'Etc/GMT+6')] <- NA

us_wnd_long$value[us_wnd_long$datetime >= as.POSIXct('2022-03-07 13:15', tz = 'Etc/GMT+6') & 
                    us_wnd_long$datetime <= as.POSIXct('2022-03-09 15:00', tz = 'Etc/GMT+6')] <- NA

us_wnd_long$value[us_wnd_long$datetime >= as.POSIXct('2022-03-19 22:45', tz = 'Etc/GMT+6') & 
                    us_wnd_long$datetime <= as.POSIXct('2022-03-20 01:30', tz = 'Etc/GMT+6')] <- NA

us_wnd_long$value[us_wnd_long$datetime >= as.POSIXct('2022-03-04 16:00', tz = 'Etc/GMT+6') & 
                    us_wnd_long$datetime <= as.POSIXct('2022-03-04 17:15', tz = 'Etc/GMT+6')] <- NA

us_wnd_long$value[us_wnd_long$datetime == as.POSIXct('2022-04-04 19:00', tz = 'Etc/GMT+6')] <- NA

us_wnd_long$value[us_wnd_long$datetime >= as.POSIXct('2022-05-09 03:00', tz = 'Etc/GMT+6') & 
                    us_wnd_long$datetime <= as.POSIXct('2022-05-09 10:45', tz = 'Etc/GMT+6')] <- NA

us_wnd_long$value[us_wnd_long$datetime >= as.POSIXct('2022-05-19 19:15', tz = 'Etc/GMT+6') & 
                us_wnd_long$datetime <= as.POSIXct('2022-05-20 12:15', tz = 'Etc/GMT+6')] <- NA

us_wnd_long$value[us_wnd_long$datetime >= as.POSIXct('2022-05-19 19:15', tz = 'Etc/GMT+6') & 
                    us_wnd_long$datetime <= as.POSIXct('2022-05-20 12:15', tz = 'Etc/GMT+6')] <- NA

us_wnd_long$value[us_wnd_long$datetime >= as.POSIXct('2022-06-13 16:45', tz = 'Etc/GMT+6') & 
                    us_wnd_long$datetime <= as.POSIXct('2022-06-14 16:15', tz = 'Etc/GMT+6')] <- NA

us_wnd_long$value[us_wnd_long$datetime >= as.POSIXct('2022-09-09 03:00', tz = 'Etc/GMT+6') & 
                    us_wnd_long$datetime <= as.POSIXct('2022-09-09 09:45', tz = 'Etc/GMT+6')] <- NA

us_wnd_long$value[us_wnd_long$datetime == as.POSIXct('2022-09-19 17:15', tz = 'Etc/GMT+6')] <- NA

us_wnd_long$value[us_wnd_long$datetime == as.POSIXct('2022-09-20 11:15', tz = 'Etc/GMT+6')] <- NA



# manual wind qaqc 2023 wat yr ----

us_wnd_long$value[us_wnd_long$datetime == as.POSIXct('2022-10-21 07:30', tz = 'Etc/GMT+6')] <- NA

us_wnd_long$value[us_wnd_long$datetime == as.POSIXct('2022-10-21 08:15', tz = 'Etc/GMT+6')] <- NA

us_wnd_long$value[us_wnd_long$datetime >= as.POSIXct('2022-10-22 05:00', tz = 'Etc/GMT+6') & 
                    us_wnd_long$datetime <= as.POSIXct('2022-10-24 04:30', tz = 'Etc/GMT+6')] <- NA

us_wnd_long$value[us_wnd_long$datetime >= as.POSIXct('2022-10-27 12:45', tz = 'Etc/GMT+6') & 
                    us_wnd_long$datetime <= as.POSIXct('2022-10-28 21:00', tz = 'Etc/GMT+6')] <- NA

us_wnd_long$value[us_wnd_long$datetime >= as.POSIXct('2022-10-29 14:45', tz = 'Etc/GMT+6') & 
                us_wnd_long$datetime <= as.POSIXct('2022-10-28 21:00', tz = 'Etc/GMT+6')] <- NA

us_wnd_long$value[us_wnd_long$datetime >= as.POSIXct('2022-10-31 03:45', tz = 'Etc/GMT+6') & 
                    us_wnd_long$datetime <= as.POSIXct('2022-11-03 12:45', tz = 'Etc/GMT+6')] <- NA

us_wnd_long$value[us_wnd_long$datetime >= as.POSIXct('2022-11-05 12:00', tz = 'Etc/GMT+6') & 
                    us_wnd_long$datetime <= as.POSIXct('2022-11-06 17:00', tz = 'Etc/GMT+6')] <- NA

us_wnd_long$value[us_wnd_long$datetime >= as.POSIXct('2022-11-11 04:30', tz = 'Etc/GMT+6') & 
                    us_wnd_long$datetime <= as.POSIXct('2022-11-11 12:00', tz = 'Etc/GMT+6')] <- NA

us_wnd_long$value[us_wnd_long$datetime >= as.POSIXct('2022-11-16 14:30', tz = 'Etc/GMT+6') & 
                    us_wnd_long$datetime <= as.POSIXct('2022-11-16 18:00', tz = 'Etc/GMT+6')] <- NA

us_wnd_long$value[us_wnd_long$datetime >= as.POSIXct('2022-11-27 10:30', tz = 'Etc/GMT+6') & 
                    us_wnd_long$datetime <= as.POSIXct('2022-12-01 01:45', tz = 'Etc/GMT+6')] <- NA

us_wnd_long$value[us_wnd_long$datetime >= as.POSIXct('2022-12-15 03:00', tz = 'Etc/GMT+6') & 
                    us_wnd_long$datetime <= as.POSIXct('2022-12-15 10:45', tz = 'Etc/GMT+6')] <- NA

us_wnd_long$value[us_wnd_long$datetime >= as.POSIXct('2022-12-18 17:30', tz = 'Etc/GMT+6') & 
                    us_wnd_long$datetime <= as.POSIXct('2022-12-23 12:45', tz = 'Etc/GMT+6')] <- NA

us_wnd_long$value[us_wnd_long$datetime >= as.POSIXct('2023-02-20 01:15', tz = 'Etc/GMT+6') & 
                    us_wnd_long$datetime <= as.POSIXct('2023-02-21 16:30', tz = 'Etc/GMT+6')] <- NA

us_wnd_long$value[us_wnd_long$datetime >= as.POSIXct('2023-02-22 01:45', tz = 'Etc/GMT+6') & 
                    us_wnd_long$datetime <= as.POSIXct('2023-02-24 11:15', tz = 'Etc/GMT+6')] <- NA

us_wnd_long$value[us_wnd_long$datetime >= as.POSIXct('2023-02-26 19:00', tz = 'Etc/GMT+6') & 
                    us_wnd_long$datetime <= as.POSIXct('2023-02-27 21:45', tz = 'Etc/GMT+6')] <- NA

us_wnd_long$value[us_wnd_long$datetime >= as.POSIXct('2023-02-28 10:45', tz = 'Etc/GMT+6') & 
                    us_wnd_long$datetime <= as.POSIXct('2023-02-28 11:30', tz = 'Etc/GMT+6')] <- NA

us_wnd_long$value[us_wnd_long$datetime >= as.POSIXct('2023-03-13 21:00', tz = 'Etc/GMT+6') & 
                    us_wnd_long$datetime <= as.POSIXct('2023-03-14 13:30', tz = 'Etc/GMT+6')] <- NA

us_wnd_long$value[us_wnd_long$datetime >= as.POSIXct('2023-03-24 15:45', tz = 'Etc/GMT+6') & 
                    us_wnd_long$datetime <= as.POSIXct('2023-03-25 10:15', tz = 'Etc/GMT+6')] <- NA

us_wnd_long$value[us_wnd_long$datetime >= as.POSIXct('2023-03-25 22:45', tz = 'Etc/GMT+6') & 
                    us_wnd_long$datetime <= as.POSIXct('2023-03-26 10:45', tz = 'Etc/GMT+6')] <- NA

us_wnd_long$value[us_wnd_long$datetime >= as.POSIXct('2023-03-27 08:15', tz = 'Etc/GMT+6') & 
                    us_wnd_long$datetime <= as.POSIXct('2023-03-27 14:15', tz = 'Etc/GMT+6')] <- NA

us_wnd_long$value[us_wnd_long$datetime >= as.POSIXct('2023-04-01 21:00', tz = 'Etc/GMT+6') & 
                    us_wnd_long$datetime <= as.POSIXct('2023-04-05 14:30', tz = 'Etc/GMT+6')] <- NA

us_wnd_long$value[us_wnd_long$datetime >= as.POSIXct('2023-04-11 01:15', tz = 'Etc/GMT+6') & 
                    us_wnd_long$datetime <= as.POSIXct('2023-04-13 12:00', tz = 'Etc/GMT+6')] <- NA

us_wnd_long$value[us_wnd_long$datetime >= as.POSIXct('2023-06-20 10:30', tz = 'Etc/GMT+6') & 
                    us_wnd_long$datetime <= as.POSIXct('2023-06-20 10:45', tz = 'Etc/GMT+6')] <- NA

us_wnd_long$value[us_wnd_long$datetime == as.POSIXct('2023-06-21 06:45', tz = 'Etc/GMT+6')] <- NA

us_wnd_long |>
  filter(name %in% ffr_sel_wind) |>
  select(datetime, name, value) |>
  rbind(pwl |> pivot_longer(!datetime) |> filter(name %in% pwl_sel_wind)) |>
  rbind(df_long |> filter(name %in% c('WindSpeed3cup_Avg', 'low_ec_wnd_spd', 'top_ec_wnd_spd')) |>  select(names(us_wnd_long))) |>
  ggplot(aes(datetime, value, colour = name)) +
  geom_line()

ggplotly()

## create output df ####

us_winds <- us_wnd_long$name |> unique()

df_out <- df_long |> 
  filter(!name %in% us_winds) |> 
  rbind(us_wnd_long) |> 
  distinct() |> 
  pivot_wider(names_from = name, values_from = value) |> 
  # set US winds to nan if speed is nan
  mutate(
    US_WindSpeed_avg_dir = ifelse(is.na(US_WindSpeed_avg), NA, US_WindSpeed_avg_dir),
    US_WindSpeed_avg_dir_std = ifelse(is.na(US_WindSpeed_avg), NA, US_WindSpeed_avg_dir_std),
    US_WindSpeed_max = ifelse(is.na(US_WindSpeed_avg), NA, US_WindSpeed_max),
    US_WindSpeed_std = ifelse(is.na(US_WindSpeed_avg), NA, US_WindSpeed_std)
  )

df_out_complete <- left_join(complete_datetime, df_out)

# ggplot(df_out, aes(x = datetime, y = SnowDepth)) +
#     geom_point()

# df_out |>
#   ggplot(aes(x = datetime, y = AirTC_midTree)) +
#   geom_point()

saveRDS(df_out_complete, 'data/ffr_met_main_qaqc.rds')


