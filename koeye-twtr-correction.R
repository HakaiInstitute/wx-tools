library(dplyr)
library(ggplot2)
library(tidyr)
library(Metrics)  # For RMSE and MAE
library(plotly)   # For ggplotly
library(lubridate) # For date handling

# Check working directory
getwd()

# Load data
df <- read.csv("data-input/ssnkoeye_pt.csv", header = FALSE, skip = 4, stringsAsFactors = FALSE)

# Load column headers and apply them
fileheaders <- read.csv("data-input/ssnkoeye_pt.csv", nrows = 1, as.is = TRUE, header = FALSE)
colnames(df) <- fileheaders


# Clean up data
df <- df %>%
  select(timestamp = 1, contains("Avg")) %>% 
  slice(-(1:59472)) # Drop rows by position
#Drop by column position
df = select(df, -c(2))

# Create datetime column based on start datetime and interval
start_datetime <- ymd_hms("2018-03-25 00:00:00")  # Update as needed
interval_minutes <- 5  # Interval in minutes
num_rows <- nrow(df)

df <- df %>%
  mutate(date = seq(from = start_datetime, by = paste(interval_minutes, "mins"), length.out = num_rows),
         date = as.POSIXct(date))

# Rename columns for consistency
colnames(df)[2:3] <- c("sensor2", "sensor3")

# Filter overlapping periods where both sensors have data
df_overlap <- df %>% filter(!is.na(sensor2) & !is.na(sensor3))

# Build linear model to correct Sensor 2 based on Sensor 3
model <- lm(sensor3 ~ sensor2, data = df_overlap)

# Evaluate model performance (R2, RMSE, MAE)
evaluate_model <- function(model, actual, predicted) {
  list(
    R2 = summary(model)$r.squared,
    RMSE = rmse(actual, predicted),
    MAE = mae(actual, predicted)
  )
}

# Predictions and corrections for Sensor 2
df_overlap$predicted_sensor2 <- predict(model, newdata = df_overlap)
performance <- evaluate_model(model, df_overlap$sensor3, df_overlap$predicted_sensor2)
print(performance)  # Output R2, RMSE, MAE

# Correct Sensor 2 values in the main dataframe
df <- df %>%
  mutate(sensor2_corrected = ifelse(is.na(sensor2), predict(model, newdata = df), sensor2))

# Residual plot for model errors
ggplot(df_overlap, aes(x = sensor2, y = sensor3 - predicted_sensor2)) +
  geom_point(size = 1, alpha = 0.4) +
  geom_hline(yintercept = 0, linetype = 'dashed', color = 'red') +
  labs(title = 'Residual Plot: Sensor 2 vs Sensor 3', x = 'Sensor 2', y = 'Residuals') +
  theme_minimal()

# Reshape data for time series plot
df_long_filtered <- df %>%
  filter(!is.na(sensor2_corrected) & !is.na(sensor3)) %>%
  gather(key = "sensor", value = "temperature", sensor2_corrected, sensor3)

# Plot time series comparison between Sensor 2 (corrected) and Sensor 3
ggplot(df_long_filtered, aes(x = timestamp, y = temperature, color = sensor, group = sensor)) +
  geom_line() + 
  scale_color_manual(values = c('sensor2_corrected' = 'purple', 'sensor3' = 'red')) + 
  labs(title = 'Time Series of Water Temperature from Sensors', 
       x = 'Timestamp', y = 'Temperature (°C)', color = 'Sensors') +
  theme_minimal() +
  theme(legend.position = 'top')

df_overlap <- df_overlap %>%
  mutate(residual = sensor3 - predicted_sensor2,
         abs_residual = abs(residual))  # Absolute residuals for ranking

# Define threshold for highest residuals (e.g., top 5%)
threshold <- quantile(df_overlap$abs_residual, 0.95, na.rm = TRUE)

# Add flag for high residuals
df_overlap <- df_overlap %>%
  mutate(high_residual = ifelse(abs_residual >= threshold, "High Residual", "Normal"))

# Plot residuals, highlighting high residual records
ggplot(df_overlap, aes(x = sensor2, y = residual, color = high_residual)) +
  geom_point(size = 1, alpha = 0.4) +
  geom_hline(yintercept = 0, linetype = 'dashed', color = 'red') +
  scale_color_manual(values = c("Normal" = "gray", "High Residual" = "red")) +
  labs(title = "Residual Plot: Sensor 2 vs Sensor 3", 
       x = "Sensor 2 Water Level", 
       y = "Residuals (m)", 
       color = "Residual Level") +
  theme_minimal() +
  theme(legend.position = "top")

