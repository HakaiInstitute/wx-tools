# apply wind speed correction for stand pipe precipitation values based on yang 1998

library(dplyr)
library(tidyr)
library(lubridate)
library(plotly)

df_windcor<-df %>% 
  select("Date", "Month", "Year", "WaterYear", contains("TAir"&"Dir"))





wtr_yr <- function(dates, start_month=10) {
  # Convert dates into POSIXlt
  dates.posix = as.POSIXlt(dates)
  # Year offset
  offset = ifelse(dates.posix$mon >= start_month - 1, 1, 0)
  # Water year
  adj.year = dates.posix$year + 1900 + offset
  # Return the water year
  adj.year
}

# bring in corrected pc, temp, wind speed for wind speed correction. All 0.00's have been removed from air temp and quick clean of wind spd was completed. There are a few major data gaps.
# Wind Speed: 2017-07-14 12:00 to 2017-07-30 22:00; 2017-07-31 16:00 to 2017-08-13 11:00;2017-08-15 01:00 to 2017-08-31 10:00; 2018-12-15 19:00 to 2019-01-13 07:00
# numerous gaps for air temp... 


df<- df %>% mutate(watyr = wtr_yr(Date, 10))  

#df %>% 
  #filter(is.na(PC) == FALSE,
         #watyr != 2020) %>% 
  #mutate(TA = if_else(is.na(TA) == T, mean(lag(TA) + lead(TA)), TA), # fill gaps
         #Wind = if_else(is.na(Wind) == T, mean(lag(Wind) + lead(Wind)), Wind)) 

 df<-df %>% 
  mutate(PP = if_else(is.na(lag(PC)) == F, PC - lag(PC), PC),
         PP = if_else(PP > 0, PP, 0)) %>% 
  mutate(corr_factor = 
           100/(
             if_else(TA < -1, (exp(4.606-0.157*(Wind^1.28))), if_else(TA > 1, (exp(4.605-0.062*(Wind^0.58))),(100.77-8.34*Wind)))
           ),
         corr_factor = if_else(corr_factor > 0, corr_factor, 1)) %>% 
  mutate(PP_corr = if_else(is.na(corr_factor) == F, PP * corr_factor, PP)) %>% 
  group_by(watyr) %>% # cumsum by wat yr
  mutate(PC_corr = cumsum(PP_corr))

write.csv(df_cor, "../buxton_PC_corrected_Wind_Factoreh.csv")


df_plot <- df_cor %>% 
  select(datetime, PC, PC_corr) %>% 
  pivot_longer(PC:PC_corr) 

  
plot_ly(
  data = newdf,
  x = ~Date,
  y = ~WindDirBuxtonEastAvg,
  #color = ~name,
  #type = "scatter",
  mode = "lines"
)
