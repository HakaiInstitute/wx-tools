library(dplyr)
library(lubridate)
# Load data
df <- read.csv("ssnkoeye_pt_raw.csv", header = FALSE, skip = 4, stringsAsFactors = FALSE)
fileheaders <- read.csv("ssnkoeye_pt_raw.csv", nrows = 1, as.is = TRUE, header = FALSE)
colnames(df) <- fileheaders

# Clean up data
df <- df %>%
  select(timestamp = 1, contains("Avg"))
# Convert timestamp to datetime format
df$timestamp <- ymd_hms(df$timestamp)

# Rename columns for consistency
## water level 2 = hobo
## water level 3 = pls
colnames(df)[2:3] <- c("water_level_sensor2", "water_level_sensor3")

df <- df %>%
  mutate(
    # Ensure timestamp is stored in UTC
    timestamp = force_tz(timestamp, "UTC")
  ) %>%
  arrange(timestamp) %>% # Ensure data is sorted by timestamp
  mutate(
    # Adjust sensor 3 readings before the given timestamp
    water_level_sensor3 = ifelse(timestamp < as.POSIXct("2024-02-12 11:30:00", tz = "UTC"), 
                                 water_level_sensor3 - 0.0405, 
                                 water_level_sensor3),
    
    # Calculate the difference between consecutive readings
    diff = c(NA, diff(water_level_sensor3)),
    
    # Assign the exceedance flag
    qflag = ifelse(abs(diff) > 0.05, "SVD: Rate of change exceedance >0.05m", NA),
    
    # Assign specific flags for known QC cases
    qflag = case_when(
      timestamp == as.POSIXct("2022-07-04 15:05:00", tz = "UTC") ~ "SVD: Sensor pre-deployment: QC'd by EH",
      timestamp == as.POSIXct("2022-08-05 20:45:00", tz = "UTC") ~ "SVD: Power cycling issues: QC'd by EH",
      timestamp == as.POSIXct("2022-10-12 11:45:00", tz = "UTC") ~ "SVD: Power cycling issues: QC'd by EH",
      timestamp >= as.POSIXct("2023-04-08 13:05:00", tz = "UTC") & 
        timestamp <= as.POSIXct("2023-04-08 13:45:00", tz = "UTC") ~ "SVD: Power cycling: QC'd by EH",
      timestamp >= as.POSIXct("2024-01-30 08:30:00", tz = "UTC") & 
        timestamp <= as.POSIXct("2024-01-30 09:10:00", tz = "UTC") ~ "SVD: Power cycling: QC'd by EH",
      timestamp >= as.POSIXct("2024-02-01 18:05:00", tz = "UTC") & 
        timestamp <= as.POSIXct("2024-02-03 22:30:00", tz = "UTC") ~ "SVD: Power cycling: QC'd by EH",
      TRUE ~ qflag # Preserve existing values
    )
  ) %>%
  # Set values associated with "SVD" flags to NA
  mutate(
    water_level_sensor3 = ifelse(!is.na(qflag) & grepl("SVD", qflag), NA, water_level_sensor3),
    
    # Only assign "AV: QC'd by EH" to non-NA records
    qflag = case_when(
      is.na(qflag) & !is.na(water_level_sensor3) ~ "AV: QC'd by EH",
      TRUE ~ qflag
    )
  ) %>%
  select(-diff) # Remove the temporary diff column

library(ggplot2)
library(dplyr)

# Filter out NA values and create the plot
df %>%
  filter(!is.na(water_level_sensor3)) %>%  # Remove NA values from water_level_sensor3
  ggplot(aes(x = timestamp, y = water_level_sensor3, color = qflag)) +
  geom_line() +  # Line graph to show the time series
  geom_point() + # Optional: Points on the graph for emphasis
  scale_color_manual(values = c(
    "SVD: Rate of change exceedance >0.05m" = "red",
    "SVD: Power cycling issues: QC'd by EH" = "blue",
    "TEST" = "green",
    "AV: QC'd by EH" = "orange"
  )) + # Customize the colors for different flags
  labs(
    title = "Water Level Sensor 3 Time Series",
    x = "Timestamp",
    y = "Water Level Sensor 3",
    color = "QC Flags"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",  # Move the legend to the top
    axis.text.x = element_text(angle = 45, hjust = 1)  # Angle x-axis labels for better readability
  )

write.csv(df, "ssnkoeye_pt.csv")
