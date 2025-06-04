#Calculate water year as numeric variable
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

#run function and add as new column

df_wtryr<-df_mm %>% mutate(watyr = wtr_yr(Date, 10)) 


depth_wtryr<- depth_long %>% mutate(watyr = wtr_yr(Date, 10)) 

df_wtryr<- data %>% mutate(watyr = wtr_yr(Date, 10))  
