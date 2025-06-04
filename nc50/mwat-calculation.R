# Step 1: Convert "timestamp" to a proper datetime format
data <- df %>%
  mutate(timestamp = ymd_hms(timestamp))

# Step 2: Calculate the daily mean temperature
daily_means <- data %>%
  mutate(date_only = as.Date(timestamp)) %>%  # Extract the date without time
  group_by(date_only) %>%                     # Group by date
  summarise(daily_mean = mean(blac_outlet1_t1_21729199))   # Calculate the mean temperature per day

# Step 3: Calculate the 7-day running average of daily means
seven_day_avg <- daily_means %>%
  mutate(seven_day_avg = zoo::rollapply(daily_mean, width = 7, mean, fill = NA, align = "right"))

# Step 4: Find the annual maximum of the 7-day running averages
mwat <- max(seven_day_avg$seven_day_avg, na.rm = TRUE)

# Display the result
print(paste("The MWAT (Maximum Weekly Average Temperature) is:", round(mwat, 2)))

# Optional: Plot the 7-day running average to visualize the trend
library(ggplot2)
ggplot(seven_day_avg, aes(x = date_only, y = seven_day_avg)) +
  geom_line(color = "blue") +
  labs(
    title = "7-Day Running Average of Daily Mean Temperature",
    x = "Date",
    y = "7-Day Average Temperature (°C)"
  ) +
  theme_minimal()

# Step 4: Find the MWAT value and its corresponding date
mwat_row <- seven_day_avg %>%
  filter(seven_day_avg == max(seven_day_avg, na.rm = TRUE)) %>%
  slice(1) # In case of ties, take the first occurrence

mwat <- mwat_row$seven_day_avg
mwat_date <- mwat_row$date_only

# Display the result
print(paste("The MWAT (Maximum Weekly Average Temperature) is:", round(mwat, 2)))
print(paste("The MWAT occurred on:", mwat_date))
