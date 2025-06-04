# Assuming your data is stored in a data frame named 'df'
# Convert the 'date' column to a Date object
depth_2022$date <- as.Date(depth_2022$date)

# Aggregate to daily timestep using dplyr and omit NA values
daily_data <- depth_2022 %>%
  group_by(date) %>%
  summarize(mean_value = ifelse(all(is.na(value)), NA, mean(value, na.rm = TRUE)))

# Print the aggregated data
print(daily_data)
