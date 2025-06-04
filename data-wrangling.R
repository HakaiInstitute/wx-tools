######################################################################################################
#Data wrangling
#################################################################################################
lapply(c("tidyverse", "lubridate", "reshape2", "stringr", "plotly", "roll", "data.table", "clifro", "scales","chron"), library, character.only = TRUE)# Make sure date is in right format and change column name 
install.packages(c("tidyverse", "lubridate", "reshape", "stringr", "plotly", "roll", "data.table", "clifro", "scales", "chron"))

df<-koeye_timeseries_2023

str(df)
colnames(df3)[1] <- "timestamp"
colnames(df)[2] <- "koeye_ssn"
colnames(df)[3] <- "SSN1015"
colnames(df)[4] <- "SSN626"
colnames(df)[5] <- "SSN844"
colnames(df)[6] <- "SSN703"
colnames(df)[7] <- "SSN703b"
colnames(df)[8] <- "koeyept1"
colnames(df)[9] <- "koeyept2"

df<-tolower(df)

library(lubridate)
g2025$Date <- as.POSIXct(g2025$Date, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
str(g2025)
df$date<-as.POSIXct(df$date,format="%Y-%m-%d %H:%M")
df_month$Date<-as.POSIXct(df_month$date,format="%Y-%m-%d")
df$Date<-as.Date(df$Date, format="%Y-%m-%d %H:%M")
str(df_long)
str(df_wtryr)
colnames(df_long)[1] <- "Date"
LULL1_PT_20409275_20220502_test$Date<-as.POSIXct(LULL1_PT_20409275_20220502_test$Date,format="%M/%d/%Y %H:%M")
data_raw$Date<-as.POSIXct(data_raw$Date,format="%Y-%m-%d")


#extract and create new month column
df_month$month <- format(as.Date(df_month$Date), "%m-%y")
str(df)

#extract and create month column written form
df$myyear<-year(df$mydate)
df$mymonth<-month(df$Date)
mymonths <- c("Jan","Feb","Mar",
              "Apr","May","Jun",
              "Jul","Aug","Sep",
              "Oct","Nov","Dec")

df$MonthAbb <- mymonths[ df$mymonth ]

#set month order
df$month <- factor(df$MonthAbb, levels = c("Jan","Feb","Mar",
                                           "Apr","May","Jun",
                                           "Jul","Aug","Sep",
                                           "Oct","Nov","Dec"))

str(df_wtryr)
colnames(df)[1] <- "Date"
LULL1_PT_20409275_20220502_test$Date<-as.POSIXct(LULL1_PT_20409275_20220502_test$Date,format="%M/%d/%Y %H:%M")
data_raw$Date<-as.POSIXct(data_raw$Date,format="%Y-%m-%d")

data_raw<-df

df_month$Date<-as.Date(df_month$Date)
str(df_month)
#confirm date in POSIXct or whatever date format you want to work with
str(data_raw)

#Select columns based on "contains"

df3<-df3 %>% 
  select("timestamp", contains("Avg"))

#lubridate
df_may<-df_month[month(df_month$Date) == 05, ]

new_df<-df %>% 
  select("Date","Year", "Month", contains("Avg"))

###make new date column
start_datetime <- ymd_hms("2017-08-30 12:00:00")  # Change this to your actual start date-time
interval_minutes <- 5  # Recording interval in minutes

# Determine number of data rows (excluding header)
num_rows <- nrow(df)   # Subtract 1 for the header row

# Generate timestamp column in POSIXct format
df$date <- seq(from = start_datetime, by = paste(interval_minutes, "mins"), length.out = num_rows)

# Ensure it's POSIXct
df$date <- as.POSIXct(df$date)

# View the first few rows
head(df)

#extract date -- year
df$year <- format(as.Date(df$Date, format="%Y-%m-%d"),"%Y")


#Drop by column position
df = select(df, -c(2))

#Drop by row position
df<-df[-c(1:59472),]

#Change wide to long format 

df_long<-data_raw %>%
    pivot_longer(TWtrFULL1PT_Avg:TWtrTUNA1PT_Avg,names_to="Site", values_to="Water Temperature")


df_long<-df %>%
  pivot_longer(PLS_TempAvg:TWtrKoeyePT2_Avg, names_to="Sensor", values_to="Water_Temperature")

depth_long<-depth_data %>%
    pivot_longer(DepthKoeyePT_Avg:DepthKoeyePT2_Avg,names_to="variable", values_to="value")


output <- pivot_longer(df, -c(id, group), names_to = "question", 
                       values_to = "score") %>%
  
  
  longer<-pivot_longer(df, names_pattern = "(.*)(..)$", names_to = c("limit", "name")) %>% 
  mutate(limit=ifelse(limit=="", "value", limit))

df_long <- gather(df_depth, condition, measurement,DepthLULL1PT_Q_level:DepthHEYD2PT_Avg  , factor_key=TRUE)

#define column positions
totdaily<-c(1,5,2,3,4,6)

#reorder columns within dataframe
dftot<-totdaily[ , c(1,5,2,3,4,6)]

# convert character to factor
data$Download_Period1 <- as.factor(data$Download_Period) 

df$Year <- as.factor(df$year)
#reorder factor levels
data$Download_Period <- factor(data$Download_Period, levels=c('1', '2', '3', '4', '5','6', '7', '8', '9', '10'))

df$Year <- factor(df$Year, levels=c('2017', '2018', '2019', '2020', '2021','2022', '2023'))
#Filter to specific date range
data_3<-data %>% 
  select(1:12) %>% 
  filter(Date >= as_datetime("2020-06-08 11:10:00"), Date <= as_datetime("2020-10-15 20:25:00"))

df_2017<-df %>% 
  select(1:4) %>%
    filter(Date >= as_date("2017-08-30"), Date <= as_datetime("2017-12-31")) %>% 
  pivot_longer(PLS_TempAvg:TWtrKoeyePT2_Avg, names_to="Sensor", values_to="Water Temperature")
df_2018<-df %>% 
  select(1:4) %>%
  filter(Date >= as_date("2018-01-01"), Date <= as_datetime("2018-12-31"))%>% 
  pivot_longer(PLS_TempAvg:TWtrKoeyePT2_Avg, names_to="Sensor", values_to="Water Temperature")
df_2019<-df %>% 
  select(1:4) %>%
  filter(Date >= as_date("2019-01-01"), Date <= as_datetime("2019-12-31"))%>% 
  pivot_longer(PLS_TempAvg:TWtrKoeyePT2_Avg, names_to="Sensor", values_to="Water Temperature")
df_2020<-df %>% 
  select(1:4) %>%
  filter(Date >= as_date("2020-01-01"), Date <= as_datetime("2020-12-31"))%>% 
  pivot_longer(PLS_TempAvg:TWtrKoeyePT2_Avg, names_to="Sensor", values_to="Water Temperature")
df_2021<-df %>% 
  select(1:4) %>%
  filter(Date >= as_date("2021-01-01"), Date <= as_datetime("2021-12-31"))%>% 
  pivot_longer(PLS_TempAvg:TWtrKoeyePT2_Avg, names_to="Sensor", values_to="Water Temperature")
df_2022<-df %>% 
  select(1:4) %>%
  filter(Date >= as_date("2022-01-01"), Date <= as_datetime("2022-12-31"))%>% 
  pivot_longer(PLS_TempAvg:TWtrKoeyePT2_Avg, names_to="Sensor", values_to="Water Temperature")
df_2023<-df %>% 
  select(1:4) %>%
  filter(Date >= as_date("2023-01-01"), Date <= as_datetime("2023-12-31"))%>% 
  pivot_longer(PLS_TempAvg:TWtrKoeyePT2_Avg, names_to="Sensor", values_to="Water Temperature")
str(df_2021)

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


write.csv(df_merge4, "2013-2019_SnowDepth.csv")

#Aggregate time interval
df_month<-df %>%
  group_by(Date= cut(Date, breaks="1 month")) %>% 
  summarise(RainPruth = sum(RainPruth, na.rm=TRUE), 
            RainQuadra = sum(RainQuadra, na.rm=TRUE))

df_month<-df %>%
  group_by(Date= cut(Date, breaks="1 month")) %>% 
  summarise(TairAvg = mean(TAirQuadra1_Avg, na.rm=TRUE),
            TAirPruth = mean(TAirPruthDockAvg, na.rm= TRUE))

#add year
df_month$Month <- format(as.Date(df_month$Date), "%m")
df_month$Year<- format(as.Date(df_month$Date), "%Y")

#add month


df_daily <- aggregate(df["variable"], 
                  list(hour=cut(as.POSIXct(df$Date)-1, "24 hours")),
                  mean, na.rm=TRUE)
str(df_daily)

df_ddw<-df_daily %>%
  group_by(Date= cut(Date, breaks="1 month")) %>% 
  mutate(ddwquadra = sum(Quadra_ddw),
         ddwpruth = sum(Pruth_ddw),
         maxQaudra = max(TainQuadraAvg),
         maxPruth = max(TairPruthAvg)) 
  
#10 min aggregation
 df_15<-df %>%
  group_by(date= cut(date, breaks="15 min")) %>%   
  summarise(PStnKoeyeAvg= mean(PStnKoeyeAvg))
 
 
 #create month abbreviation
 df_month$month<- format(as.Date(df_month$Date, format="%Y-%m-%d"),"%B")
 
#create datetime sequence
 start <- as.POSIXct("2022-07-25")
 interval <- 30
 
 end <- start + as.difftime(400, units="days")
 
 df<-data.frame('datetime' = seq(from=start, by=interval*60, to=end))
 
 #creat id column
 rowid_to_column(d, "ID")
 
write.csv(df, "tuna-time.csv")
