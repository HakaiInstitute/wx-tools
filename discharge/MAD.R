#MAD
library(dplyr)
library(ggplot2)
library(plotly)
library(lubridate)

# Load dataset (assuming it's in a data frame called df)
df <- read.csv("your_file.csv")
data844$timestamp <- ymd_hms(data844$timestamp)

# Filter for 2024 and Discharge844 site
data844 <- data %>%
    select(c(1, 2))

data626 <- data %>%
    select(c(1, 3))

# Ensure timestamp is in proper date-time format
df <- df %>%
  mutate(timestamp = ymd_hms(timestamp))

# Calculate monthly streamflow normals (mean discharge per month) for the available data
streamflow_normals_monthly <- data844 %>%
  mutate(year = year(timestamp), month = month(timestamp)) %>%
  group_by(year, month) %>%
  summarise(monthly_mean = mean(Discharge844, na.rm = TRUE)) %>%
  ungroup()

# Calculate annual streamflow normals (mean discharge per year) for the available data
streamflow_normals_annual <- data844 %>%
  mutate(year = year(timestamp)) %>%
  group_by(year) %>%
  summarise(annual_mean = mean(Discharge844, na.rm = TRUE)) %>%
  ungroup()

# Optionally: You can also calculate daily normals by extracting the date from the timestamp
streamflow_normals_daily <- df %>%
  mutate(date = as.Date(timestamp)) %>%
  group_by(date) %>%
  summarise(daily_mean = mean(discharge, na.rm = TRUE)) %>%
  ungroup()

# View the normals calculated
head(streamflow_normals_monthly)
head(streamflow_normals_annual)
head(streamflow_normals_daily)

# If you want to merge these normals back into your original dataset for comparison:
df_with_normals_monthly <- data844 %>%
  mutate(year = year(timestamp), month = month(timestamp)) %>%
  left_join(streamflow_normals_monthly, by = c("year", "month"))

data844_with_normals_annual <- data844 %>%
  mutate(year = year(timestamp)) %>%
  left_join(streamflow_normals_annual, by = "year")

df_with_normals_daily <- df %>%
  mutate(date = as.Date(timestamp)) %>%
  left_join(streamflow_normals_daily, by = "date")

# Now you can plot the data with the normals (monthly, annual, or daily) if needed

# Filter the dataset for 2024
df_2024 <- data844 %>%
  filter(year(timestamp) == 2024)

# Calculate monthly streamflow normals for 2024 (mean discharge per month)
streamflow_normals_monthly_2024 <- df_2024 %>%
  mutate(month = month(timestamp), year = year(timestamp)) %>%
  group_by(year, month) %>%
  summarise(monthly_mean = mean(Discharge844, na.rm = TRUE)) %>%
  ungroup()

# Merge the monthly normals back to the 2024 dataset
df_2024_with_normals <- df_2024 %>%
  mutate(month = month(timestamp), year = year(timestamp)) %>%
  left_join(streamflow_normals_monthly_2024, by = c("year", "month"))

# Plot the discharge time series for 2024 with monthly streamflow normals
ggplot(df_2024_with_normals, aes(x = timestamp, y = Discharge844)) +
  geom_line(color = "blue") +  # Line for discharge
  geom_line(aes(y = monthly_mean), color = "red", linetype = "dashed") +  # Monthly streamflow normals
  labs(title = "Discharge Time Series for 2024 with Monthly Streamflow Normals",
       x = "Time", y = "Discharge (m3/s)") +
  theme_minimal() +
  theme(legend.position = "none")  # Remove legend if not needed
