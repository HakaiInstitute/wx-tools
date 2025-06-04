#####################################################################################################################################################
#####################################################################################################################################################
#Data wrangling


lapply(c("tidyverse", "lubridate", "reshape2", "stringr", "plotly", "roll", "data.table"), library, character.only = TRUE)

#set working directory
setwd("C:/Users/Emily/Downloads/50-watersheds/data")

# check your working directory is in the right spot
getwd()

df <- read.csv("2024-08-30.5minuteSamples.2024.csv",
               header = FALSE,
               skip = 4,
               stringsAsFactors = FALSE)

# Load and set headers, then rename the 'Measurement Time' column
fileheaders <- read.csv("2024-08-30.5minuteSamples.2024.csv",
                        nrows = 1, header = FALSE, as.is = TRUE)
colnames(df) <- fileheaders

# Check for the exact column name to ensure accurate renaming
print(colnames(df))

# Rename the 'Measurement Time' column to 'timestamp'
colnames(df)[colnames(df) == "Measurement time"] <- "timestamp"

# Convert 'timestamp' to the desired datetime format
df$timestamp <- as.POSIXct(df$timestamp, format = "%Y-%m-%d %H:%M")

# Display the column names and glimpse of the dataframe
names(df)
glimpse(df)
str(df)

#Select columns based on "contains"

df<-df %>% 
  select("timestamp", contains("Avg"))

#aggregate to hourly for plotting

# Aggregate data to hourly average
df_hourly <- df %>%
  mutate(timestamp = floor_date(timestamp, unit = "hour")) %>%  # Round down to the nearest hour
  group_by(timestamp) %>%
  summarise(across(everything(), \(x) mean(x, na.rm = TRUE)))  # Calculate the mean for all columns

#load other dfs
adam_wx <- read.csv("adam-wx-20240815.csv",
               header = TRUE,
               stringsAsFactors = FALSE)
adam_wx$timestamp <- ymd_hm(adam_wx$timestamp) 

salmon_wx <- read.csv("salmon-wx-20240815.csv",
               header = TRUE,
               stringsAsFactors = FALSE)
salmon_wx$timestamp <- ymd_hm(salmon_wx$timestamp)

station_log <- read.csv("station_log.csv",
                    header = TRUE,
                    stringsAsFactors = FALSE)

#extract start and stop time of NC50 temp record
#create new timestamp columns for snwx columns

# Convert timestamps to UTC if timezone information is inconsistent
df_hourly <- df_hourly %>%
  mutate(timestamp = with_tz(timestamp, tzone = "America/Vancouver"))

adam_wx <- adam_wx %>%
  mutate(timestamp = with_tz(timestamp, tzone = "America/Vancouver"))

salmon_wx <- salmon_wx %>%
  mutate(timestamp = with_tz(timestamp, tzone = "America/Vancouver"))


# Ensure your df_hourly has 'timestamp' column
df_hourly <- df_hourly %>% arrange(timestamp)

# Extract start and stop times
start_time <- min(df_hourly$timestamp)
stop_time <- max(df_hourly$timestamp)

# Generate a sequence of hourly timestamps
all_timestamps <- seq(from = start_time, to = stop_time, by = "hour")

# Create a new dataframe with these timestamps
adam_wx_expanded <- tibble(timestamp = all_timestamps)
salmon_wx_expanded<-tibble(timestamp = all_timestamps)

# Merge with the original adam_wx dataframe to include NA values for missing timestamps
adam_wx_expanded <- adam_wx_expanded %>%
  left_join(adam_wx, by = "timestamp")

salmon_wx_expanded <-salmon_wx_expanded %>%
  left_join(salmon_wx, by = "timestamp")

# View the expanded dataframe
print(adam_wx_expanded)
print(salmon_wx_expanded)


###WSC dataframes
# Install and load necessary packages
if (!requireNamespace("tidyhydat", quietly = TRUE)) {
  install.packages("tidyhydat")
}
library(tidyhydat)
library(dplyr)

download_hydat()

#08HF015 Eve below Kunnum HGH
#08HD007 Salmon above Memekay HGH
#08HD006 Salmon near Sayward HGH TW
#08HF004 Tsitika below Catharine HGH TW


start_timestamp <- as.POSIXct("2024-05-03 11:00:00", tz = "UTC")

nc50_wsc <-realtime_ws(
  station_number = c("08HD006","08HD007","08HF015","08HF004"),
  parameters = c(46, 5), ## see param_id for a list of codes 5 - TW, 46 - HGH
  start_date = start_timestamp, # number of days before today
  end_date = Sys.Date()
)
#####################################################################################################################################################
#####################################################################################################################################################
#parse dataframe

# Define the patterns
patterns <- list(
  tsit = "Tsit",
  adam = "Adam",
  salm = "Salm"
)

# Function to create a dataframe based on a pattern
parse_dataframe <- function(df, pattern) {
  # Find columns that match the pattern
  cols_to_keep <- names(df)[str_detect(names(df), pattern)]
  
  # Ensure 'timestamp' column is included if it exists
  if ("timestamp" %in% names(df)) {
    cols_to_keep <- c("timestamp", cols_to_keep)
  }
  
  # Create the new dataframe
  df_subset <- df %>%
    select(all_of(cols_to_keep))
  
  return(df_subset)
}

# Parse dataframes based on patterns
df_tsit <- parse_dataframe(df, patterns$tsit)
df_adam <- parse_dataframe(df, patterns$adam)
df_salm <- parse_dataframe(df, patterns$salm)

# Print or use the dataframes as needed
print(df_tsit)
print(df_adam)
print(df_salm)

#####################################################################################################################################################
#####################################################################################################################################################

plot_Adam <- ggplot(df_hourly) +
  geom_line(aes(x = timestamp(), y = df_hourly, color = "TWtrW50_AdamOutlet1_T1_Avg" ), size = 1) +
  geom_line(aes(x = TIMESTAMP, y = AirTemp2_Avg, color = "TWtrW50_AdamOutlet1_T2_Avg"), size = 1) +
  scale_color_manual(values = c("TWtrW50_AdamOutlet1_T1_Avg" = colors[4], "TWtrW50_AdamOutlet1_T2_Avg" = colors[7])) +
  labs(title = "Air Temperature Comparison", x = "Timestamp", y = "Temperature") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot_WSC <- ggplot(nc50_wsc) +
  geom_line(aes(x = Date(), y = nc50_wsc$Value, color = "TWtrW50_AdamOutlet1_T1_Avg" ), size = 1) +
  geom_line(aes(x = TIMESTAMP, y = AirTemp2_Avg, color = "TWtrW50_AdamOutlet1_T2_Avg"), size = 1) +
  scale_color_manual(values = c("TWtrW50_AdamOutlet1_T1_Avg" = colors[4], "TWtrW50_AdamOutlet1_T2_Avg" = colors[7])) +
  labs(title = "Air Temperature Comparison", x = "Timestamp", y = "Temperature") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


