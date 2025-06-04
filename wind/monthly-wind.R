

wind_data <- char_df %>%
  mutate(month = format(date, "%Y-%m"))

# Calculate average monthly wind speed
average_monthly_wind <- wind_data %>%
  group_by(month) %>%
  summarize(average_wind_direction = mean(WindDirLookoutAvg, na.rm = TRUE))

# Display the result
print(average_monthly_wind)

write.csv(average_monthly_wind,"lookout.monthly.spd.csv")
write.csv(average_monthly_wind,"lookout.monthly.dir.csv")
