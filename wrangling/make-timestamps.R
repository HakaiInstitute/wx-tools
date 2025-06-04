# Load lubridate package for working with dates and times
library(lubridate)

# Define start and stop dates
start_date <- ymd_hms("2019-11-27 12:50:00")

stop_date <- ymd_hms("2022-01-17 10:00:00")   # Change this to your desired stop date and time

# Define start and stop dates
start_date <- ymd_hms("2021-05-05 11:00:00")  # Change this to your desired start date and time
stop_date <- ymd_hms("2022-01-17 10:00:00")   # Change this to your desired stop date and time

# Add half an hour to the stop date
stop_date <- stop_date + dhours(0.5)

# Generate timestamps at half-hour intervals between start and stop dates
timestamps <- seq(start_date, stop_date, by = "30 mins")

timestamp_df <- data.frame(Timestamp = timestamps)

# Format the timestamps to ensure midnight is printed as "00:00:00"
timestamp_df$Timestamp <- format(timestamp_df$Timestamp, "%Y-%m-%d %H:%M:%S")
write.csv(timestamp_df,"703_timestamp.csv")

# Print the data frame
print(timestamp_df)

