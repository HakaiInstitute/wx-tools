


##################################################################################################################################################################

# Load necessary packages
library(lubridate)
library(dplyr)

# Sample data
data <- data.frame(
  timestamp = c("2024-06-12T15:57:43.000Z", "2024-06-12T15:58:30.000Z", "2024-06-12T15:59:43.000Z"),
  site_id = c("SALM_TRIB4_T1", "SALM_TRIB4_T1", "SALM_TRIB4_T1"),
  temperature = c(9.31, 9.299, 9.297)
)

# Convert timestamp to POSIXct format and round to nearest 5 minutes
data$timestamp <- ymd_hms(data$timestamp, tz = "UTC")
data$rounded_timestamp <- round_date(data$timestamp, "5 minutes")

# Group by site_id and rounded_timestamp, then calculate the average temperature
averaged_data <- data %>%
  group_by(site_id, rounded_timestamp) %>%
  summarize(average_temperature = mean(temperature, na.rm = TRUE), .groups = 'drop')

# View the result
averaged_data

write.csv(averaged_data,"ysi-data.csv")
