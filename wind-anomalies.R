# Load necessary libraries
library(tidyverse)
library(zoo)

# Load wind speed data
df <- read.csv("ethel-wind.csv")

# Convert timestamp to Date-Time format
df3$timestamp <- as.POSIXct(df3$timestamp, format="%Y-%m-%d %H:%M:%S", tz="UTC")

# Sort data by time
df3 <- df3 %>% arrange(timestamp)

# Compute a long-term rolling average (1-day window)
df3 <- df3 %>%
  mutate(
    rolling_mean_1day = rollmean(WindSpdLookoutAvg, k = 288, fill = NA, align = "right"),  # 288 points = 24 hrs of 5-min data
    rolling_sd_1day = rollapply(WindSpdLookoutAvg, width = 288, FUN = sd, fill = NA, align = "right")
  )

#  Plot long-term trend of wind speed
ggplot(df3, aes(x = timestamp, y = rolling_mean_1day)) +
  geom_line(color = "blue") +
  labs(title = "Long-Term Wind Speed Trend (1-Day Rolling Average)", x = "Time", y = "Wind Speed (m/s)") +
  theme_minimal()

# Detect anomalies based on long-term trend
df3 <- df3 %>%
  mutate(
    z_score_long_term = (rolling_mean_1day - mean(rolling_mean_1day, na.rm = TRUE)) / sd(rolling_mean_1day, na.rm = TRUE),
    anomaly_long_term = ifelse(z_score_long_term < -1.5, 1, 0)  # Lower threshold for subtle shifts
  )

#  Plot anomalies in long-term trend
ggplot(df3, aes(x = timestamp, y = rolling_mean_1day)) +
  geom_line(color = "blue") +
  geom_point(data = df %>% filter(anomaly_long_term == 1), aes(x = timestamp, y = rolling_mean_1day), color = "red", size = 2) +
  labs(title = "Anomalies in Long-Term Wind Speed Trend", x = "Time", y = "Wind Speed (m/s)") +
  theme_minimal()

#  High-Wind Analysis (Only Check Speeds > 10 m/s)
df_high_wind <- df %>%
  filter(WindSpdEthelAvg > 10) %>%
  mutate(
    z_score_high_wind = (WindSpdEthelAvg - mean(WindSpdEthelAvg, na.rm = TRUE)) / sd(WindSpdEthelAvg, na.rm = TRUE),
    anomaly_high_wind = ifelse(z_score_high_wind < -1.5, 1, 0)  # Check if strong wind readings were affected
  )

# Assuming your dataframe is called 'df' and the anomaly column is 'anomaly'
# Filter the anomalies where 'anomaly' flag is 1

anomalies_df3 <- df3 %>% 
  filter(anomaly_long_term == 1)  # Extract rows with anomalies

# View the anomalies
print(anomalies_df3)

# Optionally, save the anomalies to a CSV file
write.csv(anomalies_df, "anomalies_ethel.csv", row.names = FALSE)


#  Plot anomalies only during high wind events
ggplot(df_high_wind, aes(x = timestamp, y = WindSpdEthelAvg)) +
  geom_line(color = "green") +
  geom_point(data = df_high_wind %>% filter(anomaly_high_wind == 1), aes(x = timestamp, y = WindSpdEthelAvg), color = "red", size = 2) +
  labs(title = "Anomalies in High Wind Events (Above 10 m/s)", x = "Time", y = "Wind Speed (m/s)") +
  theme_minimal()

#  Compare With Nearby Station Over Time (If Available)
comp_df <- read.csv("pruth-wind.csv") %>%
  mutate(timestamp = as.POSIXct(timestamp, format="%Y-%m-%d %H:%M:%S", tz="UTC"))

# Rename column for clarity
colnames(df2)[which(names(df2) == "WindSpdEthelAvg")] <- "WindSpdPruthAvg"

# Merge datasets
merged_df <- df %>%
  left_join(df2, by = "timestamp")

# Compute difference between anemometer and nearby station
merged_df <- merged_df %>%
  mutate(speed_diff = WindSpdEthelAvg - WindSpdPruthDockAvg)

# Plot the difference over time
ggplot(merged_df, aes(x = timestamp, y = speed_diff)) +
  geom_line(color = "purple") +
  labs(title = "Wind Speed Difference (Anemometer vs Nearby Station)", x = "Time", y = "Speed Difference (m/s)") +
  theme_minimal()
