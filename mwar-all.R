library(dplyr)
library(ggplot2)
library(zoo)
library(lubridate)

# Step 1: Convert "timestamp" to a proper datetime format
data <- df %>%
  mutate(timestamp = ymd_hms(timestamp))

# Step 2: Specify the stream columns to process (all except the date column)
stream_columns <- colnames(df)[2:9]  # Select all except the first column, which is "date"


for (stream in stream_columns) {
  # Step 1: Calculate daily mean temperature for the stream
  daily_means <- df %>%
    mutate(date_only = as.Date(timestamp)) %>%
    group_by(date_only) %>%
    summarise(daily_mean = mean(.data[[stream]], na.rm = TRUE))  # Dynamic column reference
  
  # Step 2: Calculate the 7-day running average
  seven_day_avg <- daily_means %>%
    mutate(seven_day_avg = zoo::rollapply(daily_mean, width = 7, mean, fill = NA, align = "right"))
  
  # Step 3: Find the annual maximum of the 7-day running averages
  annual_max <- seven_day_avg %>%
    mutate(year = year(date_only)) %>%
    group_by(year) %>%
    summarise(
      max_temp = max(seven_day_avg, na.rm = TRUE),
      max_date = date_only[which.max(seven_day_avg)]
    )
  
  # Save results for the stream
  results[[stream]] <- annual_max
  
  # Step 4: Plot the data with the annual maximum highlighted
  plot <- ggplot(seven_day_avg, aes(x = date_only, y = seven_day_avg)) +
    geom_line(color = "blue") +
    geom_point(
      data = annual_max,
      aes(x = max_date, y = max_temp),
      color = "red",
      size = 3
    ) +
    labs(
      title = paste("7-Day Running Average and Annual MWAT for:", stream),
      x = "Date",
      y = "7-Day Average Temperature (°C)"
    ) +
    theme_minimal()
  
  plots[[stream]] <- plot
}

# Display plots (optional, loop through for individual plots)
for (plot in plots) {
  print(plot)
}

# Display results
for (stream in stream_columns) {
  cat(paste("\nStream:", stream, "\n"))
  print(results[[stream]])
}
