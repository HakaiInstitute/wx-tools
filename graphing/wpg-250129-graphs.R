library(dplyr)
library(lubridate)
library(ggplot2)
data626 <- data626 %>%
  mutate(timestamp = as.POSIXct(timestamp))  # Ensure 'timestamp' is POSIXct format


# Step 1: Calculate the Daily Mean for the Period of Record
df_daily_mean <- data626 %>%
  mutate(date = as.Date(timestamp)) %>%  # Extract full date (year, month, day)
  mutate(month_day = format(date, "%m-%d")) %>%  # Extract month-day for grouping
  group_by(month_day) %>%  # Group by month-day (ignores year)
  summarise(daily_mean = mean(Discharge626, na.rm = TRUE)) %>%
  ungroup()

# Step 2: Filter 2024 data for discharge timeseries
df_2024 <- data626 %>%
  filter(year(timestamp) == 2024) %>%
  mutate(month_day = format(timestamp, "%m-%d"))  # Extract month-day for merging

# Step 3: Merge 2024 discharge data with the historical daily mean
df_2024_with_mean <- df_2024 %>%
  left_join(df_daily_mean, by = "month_day")


# Step 4: Create the ggplot
p <- ggplot() +
  # Plot the real-time discharge data for 2024
  geom_line(data = df_2024, aes(x = timestamp, y = Discharge626), color = "blue", size = 1) +
  # Plot the historical daily mean discharge (period of record)
  geom_line(data = df_2024_with_mean, aes(x = month_day_date, y = daily_mean), color = "red", linetype = "dashed", size = 1) +
  labs(title = "2024 Discharge Time Series with Daily Mean for the Period of Record",
       x = "Date", y = "Discharge (m3/s)") +
  theme_minimal() +
  theme(legend.position = "none")  # Remove legend if not needed

# Step 5: Convert ggplot to ggplotly for interactivity
p_interactive <- ggplotly(p)

# Step 6: Display the interactive plot
p_interactive
######################

# Step 4: Convert 'month_day' in df_2024_with_mean to proper Date format for plotting
df_2024_with_mean <- df_2024_with_mean %>%
  mutate(month_day_date = as.POSIXct(paste("2024", month_day, sep = "-"), format = "%Y-%m-%d"))  # Create Date for the 2024 year and convert to POSIXct


library(dplyr)
library(lubridate)
library(ggplot2)
library(plotly)

# Step 1: Calculate the Daily Mean for the Period of Record
df_daily_mean <- data626 %>%
  mutate(date = as.Date(timestamp)) %>%  # Extract full date (year, month, day)
  mutate(month_day = format(date, "%m-%d")) %>%  # Extract month-day for grouping
  group_by(month_day) %>%  # Group by month-day (ignores year)
  summarise(daily_mean = mean(Discharge626, na.rm = TRUE)) %>%
  ungroup()

# Step 2: Filter 2024 data for discharge timeseries
df_2024 <- data626 %>%
  filter(year(timestamp) == 2024) %>%
  mutate(month_day = format(timestamp, "%m-%d"))  # Extract month-day for merging

# Step 3: Merge 2024 discharge data with the historical daily mean
df_2024_with_mean <- df_2024 %>%
  left_join(df_daily_mean, by = "month_day")

# Step 4: Create the ggplot
p <- ggplot() +
  # Plot the real-time discharge data for 2024
  geom_line(data = df_2024, aes(x = timestamp, y = Discharge626), color = "blue", size = 1) +
  # Plot the historical daily mean discharge (period of record)
  geom_line(data = df_2024_with_mean, aes(x = as.POSIXct(paste("2024", month_day, sep = "-")), y = daily_mean), color = "red", linetype = "dashed", size = 1) +
  labs(title = "2024 Discharge Time Series with Daily Mean for the Period of Record",
       x = "Date", y = "Discharge (m3/s)") +
  theme_minimal() +
  theme(legend.position = "none")  # Remove legend if not needed

# Step 5: Convert ggplot to ggplotly for interactivity
p_interactive <- ggplotly(p)

# Step 6: Display the interactive plot
p_interactive
