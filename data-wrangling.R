######################################################################################################
#Data wrangling
#################################################################################################
lapply(c("tidyverse", "lubridate", "reshape", "stringr", "plotly", "roll", "data.table", "clifro"), library, character.only = TRUE)# Make sure date is in right format and change column name 

<<<<<<< HEAD
str(data_raw)
colnames(data_raw)[1] <- "Date"
data_raw$Date<-as.POSIXct(data_raw$Date,format="%Y-%m-%d %H:%M")
=======
str(data)
colnames(df)[1] <- "Date"
new_df$Date<-as.POSIXct(df$Date,format="%Y-%m-%d %H:%M")
>>>>>>> 16449a7ce0b585d20c70a14470845787e1770e28

#confirm date in POSIXct or whatever date format you want to work with
str(data_raw)

#Select columns based on "contains"
<<<<<<< HEAD
data_twtr<-data_twtr %>% 
  select("Date","Year", "Month", contains("Avg"))
=======
new_df<-df_wtryr %>% 
  select("Date","Year", "Month", "watyr", contains("SC"))
>>>>>>> 16449a7ce0b585d20c70a14470845787e1770e28

#Drop by column position
new_df = select(df, -c(33:35))

#Drop by row position
new_df<-df[-c(1:7),]

#Change wide to long format
<<<<<<< HEAD
twtr_long<-data_twtr %>%
    pivot_longer(TWtrFULL1PT_Avg:TWtrTUNA1PT_Avg,names_to="Site", values_to="Water Temperature")
=======
depth_long<-depth_data %>%
    pivot_longer(DepthKoeyePT_Avg:DepthKoeyePT2_Avg,names_to="variable", values_to="value")
>>>>>>> 16449a7ce0b585d20c70a14470845787e1770e28

#define column positions
positions<-c(1:28)

# convert character to factor
data$Download_Period1 <- as.factor(data$Download_Period) 

#reorder factor levels
data$Download_Period <- factor(data$Download_Period, levels=c('1', '2', '3', '4', '5','6', '7', '8', '9', '10'))

#Filter to specific date range
data_3<-data %>% 
  select(1:12) %>% 
  filter(Date >= as_datetime("2020-06-08 11:10:00"), Date <= as_datetime("2020-10-15 20:25:00"))

#Aggregate time interval
df_daily<-df_long %>%
  group_by(Date= cut(Date, breaks="24 hours"))

df_daily <- aggregate(df["variable"], 
                  list(hour=cut(as.POSIXct(df$Date)-1, "24 hours")),
                  mean, na.rm=TRUE)

#extract by month 
data <- data %>%
  mutate(month = month(Date))

#Summarize by month
data_depthsum <- depth_wtryr %>%
  group_by(Site, watyr, Month) %>%
  summarise(max_depth = max(`Water Depth`, na.rm=TRUE),
            min_depth = min(`Water Depth`, na.rm=TRUE))

#Merge multiple dataframes
df_merge<-merge(BuxtonEast, Hecate, by.x = 1, by.y = 1, all.x = TRUE)
df_merge2<-merge(df_merge, RefStn, by.x=1, by.y=1, all.x=TRUE)
df_merge3<-merge(df_merge2, WSN693703, by.x=1, by.y=1, all.x=TRUE)
df_merge4<-merge(df_merge3, WSN703708, by.x=1, by.y=1, all.x=TRUE)

<<<<<<< HEAD
write.csv(df_merge4, "2013-2019_SnowDepth.csv")
=======
#Aggregate time interval
df_daily<-df_long %>%
  group_by(Date= cut(Date, breaks="24 hours"))

df_daily <- aggregate(df["variable"], 
                  list(hour=cut(as.POSIXct(df$Date)-1, "24 hours")),
                  mean, na.rm=TRUE)
>>>>>>> 16449a7ce0b585d20c70a14470845787e1770e28
