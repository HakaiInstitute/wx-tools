# Load required libraries
library(dplyr)

# Convert 'date' column to datetime object
weather_data$date <- as.POSIXct(weather_data$date)

# Extract date component
df$date_only <- as.Date(df$date)

df$windspdorfordcampavg_kmh <- df$windspdorfordcampavg * 3.6


# Group data by date
daily_summary <- df %>%
  group_by(date_only) %>%
  summarise(
    sum_rain1hr = sum(rain1hrorfordcamp, na.rm = TRUE),
    avg_tair = mean(tairorfordcampavg, na.rm = TRUE),
    avg_rhorfordcamp = mean(rhorfordcampavg, na.rm = TRUE),
    avg_winddir = mean(winddirorfordcampavg, na.rm = TRUE),
    avg_windspd = mean(windspdorfordcampavg_kmh, na.rm = TRUE)
  )

# Print daily summaries
print(daily_summary)

library(openair)
# Load required libraries
library(openair)

# Create wind rose plot
windRose(daily_summary, type = "default", ws = "windspdorfordcampavg_kmh", wd = "winddirorfordcampavg",
         key.header = "Wind Speed (m/s)", key.footer = "Frequency (%)", heading = "Wind Rose Plot")

# Assuming your wind direction column is named 'winddirorfordcampavg'
# If your data uses a different column name, adjust accordingly

# Step 1: Frequency Calculation
wind_direction_freq <- table(df$winddirorfordcampavg)

# Step 2: Find the Mode (Most Frequent Wind Direction)
prevailing_wind_direction <- names(wind_direction_freq)[which.max(wind_direction_freq)]

# Print the prevailing wind direction
print(prevailing_wind_direction)

#########################
#plots
########################
df <- df %>%
  filter(date < as.POSIXct("2023-10-12 14:55:00") | date > as.POSIXct("2023-10-20 10:55:00"))

# Rain 1hr plot
p1 <- ggplot(weather_data, aes(x = date, y = rain1hrorfordcamp)) +
  geom_line(color = colors[1]) +
  labs(x = "Date", y = "Rain (1hr)") +
  theme_minimal()

# Air temperature plot
p2 <- ggplot(weather_data, aes(x = date, y = tairorfordcampavg)) +
  geom_line(color = colors[2]) +
  labs(x = "Date", y = "Air Temperature (°C)") +
  theme_minimal()

# Relative humidity plot
p3 <- ggplot(weather_data, aes(x = date, y = rhorfordcampavg)) +
  geom_line(color = colors[3]) +
  labs(x = "Date", y = "Relative Humidity (%)") +
  theme_minimal()

# Wind direction plot
p4 <- ggplot(weather_data, aes(x = date, y = winddirorfordcampavg)) +
  geom_line(color = colors[4]) +
  labs(x = "Date", y = "Wind Direction (°)") +
  theme_minimal()

# Wind speed plot
p5 <- ggplot(weather_data, aes(x = date, y = windspdorfordcampavg_kmh)) +
  geom_line(color = colors[5]) +
  labs(x = "Date", y = "Wind Speed (km/h)") +
  theme_minimal()

# Arrange plots in a grid
grid.arrange(p1, p2, p3, p4, p5, ncol = 1)
