library(dplyr)
library(ggplot2)
library(tidyr)
library(Metrics)  # For RMSE and MAE
library(lubridate)  # For handling datetime

## water level 2 = hobo or KR01_PT2 
## water level 3 = pls

# Load data
df <- read.csv("ssnkoeye_pt.csv", header = FALSE, skip = 4, stringsAsFactors = FALSE)
fileheaders <- read.csv("ssnkoeye_pt.csv", nrows = 1, as.is = TRUE, header = FALSE)
colnames(df) <- fileheaders


# Adjust Sensor 2 to correct for elevation difference
#df <- df %>%
 # mutate(water_level_sensor2_adj = abs(water_level_sensor2 - 0.135)) 

# Identify overlapping periods where both sensors have values
df_overlap <- df %>%
  filter(!is.na(water_level_sensor2) & !is.na(water_level_sensor3))

# Build regression model to adjust Sensor 2 based on Sensor 3
model <- lm(water_level_sensor3 ~ water_level_sensor2, data = df_overlap)
df_overlap$predicted_water_level_sensor2 <- predict(model, newdata = df_overlap)

# Calculate raw RMSE and R²
raw_rmse <- rmse(df_overlap$water_level_sensor3, df_overlap$water_level_sensor2)
raw_r2 <- summary(model)$r.squared
cat("Overall RMSE:", round(raw_rmse, 3), "\n")
cat("R-squared:", round(raw_r2, 3), "\n")

# Calculate overall RMSE and R²
overall_rmse <- rmse(df_overlap$water_level_sensor3, df_overlap$predicted_water_level_sensor2)
overall_r2 <- summary(model)$r.squared
cat("Overall RMSE:", round(overall_rmse, 3), "\n")
cat("R-squared:", round(overall_r2, 3), "\n")


# Apply the model to all sensor 2 values
df <- df %>%
  mutate(water_level_sensor2_corrected = predict(model, newdata = df))

# Scatter Plot: raw sensor 2 vs sensor 3
ggplot(df_overlap, aes(x = water_level_sensor2, y = water_level_sensor3)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Sensor 2 vs Sensor 3",
       x = "Sensor 2 (Raw)", y = "Sensor 3 (Raw)") +
  annotate("text", x = min(df_overlap$water_level_sensor2), 
           y = max(df_overlap$water_level_sensor3), 
           label = paste("RMSE:", round(raw_rmse, 3), "\nR²:", round(raw_r2, 3)), 
           hjust = 0, size = 5, color = "black") +
  theme_minimal()

# Scatter Plot: Modelled sensor 2 vs sensor 3
ggplot(df_overlap, aes(x = predicted_water_level_sensor2, y = water_level_sensor3)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Modelled vs Actual Water Level",
       x = "Sensor 2 (Modelled)", y = "Sensor 3 (Actual)") +
  annotate("text", x = min(df_overlap$water_level_sensor2), 
           y = max(df_overlap$water_level_sensor3), 
           label = paste("RMSE:", round(overall_rmse, 3), "\nR²:", round(overall_r2, 3)), 
           hjust = 0, size = 5, color = "black") +
  theme_minimal()

# Aggregate to daily timestep
df_daily <- df_overlap %>%
  mutate(date_only = as.Date(timestamp)) %>%
  group_by(date_only) %>%
  summarise(
    sensor2_daily = mean(water_level_sensor2, na.rm = TRUE),
    sensor2_daily_predicted = mean(predicted_water_level_sensor2, na.rm = TRUE),
    sensor3_avg = mean(water_level_sensor3, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = c(sensor2_daily, sensor2_daily_predicted, sensor3_avg), 
               names_to = "sensor_type", values_to = "water_level")

# Time Series Plot: Daily Aggregated Water Levels
ggplot(df_daily, aes(x = date_only, y = water_level, color = sensor_type, group = sensor_type)) +
  geom_line() +
  scale_color_manual(values = c("sensor2_daily" = "blue", 
                                "sensor2_daily_predicted" = "red", 
                                "sensor3_avg" = "green")) +
  labs(title = "Daily Time Series of Water Levels",
       x = "Date", y = "Water Level (m)", color = "Sensor Type") +
  theme_minimal() +
  theme(legend.position = "top")

# Calculate residuals for both models
df_overlap <- df_overlap %>%
  mutate(
    residual_uncorrected = water_level_sensor2 - water_level_sensor3,
    residual_predicted = predicted_water_level_sensor2 - water_level_sensor3
  )

# Histogram of residuals: Uncorrected Sensor 2 vs. Sensor 3
ggplot(df_overlap, aes(x = residual_uncorrected)) +
  geom_histogram(binwidth = 0.002, fill = "blue", alpha = 0.6, color = "black") +
  labs(title = "Histogram of Residuals: Uncorrected Sensor 2 vs. Sensor 3",
       x = "Residual (m)", y = "Count") +
  theme_minimal()

# Histogram of residuals: Predicted Sensor 2 vs. Sensor 3
ggplot(df_overlap, aes(x = residual_predicted)) +
  geom_histogram(binwidth = 0.002, fill = "red", alpha = 0.6, color = "black") +
  labs(title = "Histogram of Residuals: Predicted Sensor 2 vs. Sensor 3",
       x = "Residual (m)", y = "Count") +
  theme_minimal()

write.csv(df, "ssnkoeye_pt_corrected.csv")
