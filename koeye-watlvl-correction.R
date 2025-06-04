library(dplyr)
library(ggplot2)
library(tidyr)
library(Metrics)  # For RMSE and MAE
library(lubridate)  # For handling datetime

# Load data
df <- read.csv("ssnkoeye_pt.csv", header = FALSE, skip = 4, stringsAsFactors = FALSE)

# Load column headers and apply them
fileheaders <- read.csv("ssnkoeye_pt.csv", nrows = 1, as.is = TRUE, header = FALSE)
colnames(df) <- fileheaders

# Clean up data
df <- df %>%
  select(timestamp = 1, contains("Avg")) %>% 
  slice(-(1:90864)) # Drop rows by position


# Convert timestamp to datetime format
df$timestamp <- ymd_hms(df$timestamp)

# Rename columns for consistency
colnames(df)[2] <- "water_level_sensor2"
colnames(df)[3] <- "water_level_sensor3"

df <- df %>% 
  mutate(water_level_sensor2_adj = abs(water_level_sensor2 - 0.135)) #correct for elevation difference between sensor 2 and sensor 3


# Identify overlapping periods where both sensors have values
df_overlap <- df %>%
  filter(!is.na(water_level_sensor2) & !is.na(water_level_sensor3))




# Build regression model to adjust elevation corrected Sensor 2 based on Sensor 3
model <- lm(water_level_sensor3 ~ water_level_sensor2, data = df_overlap)

# Predictions for correction
df_overlap$predicted_water_level_sensor2 <- predict(model, newdata = df_overlap)

# **Calculate overall RMSE and R²**
overall_rmse <- rmse(df_overlap$water_level_sensor3, df_overlap$predicted_water_level_sensor2)
overall_r2 <- summary(model)$r.squared
print(paste("Overall RMSE:", round(overall_rmse, 3)))
print(paste("R-squared:", round(overall_r2, 3)))

# Apply correction to Sensor 2 values
df <- df %>%
  mutate(water_level_sensor2_corrected = ifelse(is.na(water_level_sensor2), 
                                                predict(model, newdata = df), 
                                                water_level_sensor2))

# **Scatter Plot with RMSE and R² Annotation**
ggplot(df_overlap, aes(x = water_level_sensor2, y = water_level_sensor3)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Modeled vs Actual Water Level",
       x = "Sensor 2 (Modeled)",
       y = "Sensor 3 (Actual)") +
  annotate("text", x = min(df_overlap$water_level_sensor2), 
           y = max(df_overlap$water_level_sensor3), 
           label = paste("RMSE:", round(overall_rmse, 3), "\nR²:", round(overall_r2, 3)), 
           hjust = 0, size = 5, color = "black") +
  theme_minimal()

# **Aggregate to daily timestep**
df_daily <- df %>%
  filter(!is.na(water_level_sensor2_adj) & !is.na(water_level_sensor3)) %>%  # Corrected filter syntax
  mutate(date_only = as.Date(timestamp)) %>%
  group_by(date_only) %>%
  summarise(
    sensor2_avg = mean(water_level_sensor2, na.rm = TRUE),
    sensor2_corrected_avg = mean(water_level_sensor2_adj, na.rm = TRUE),
    sensor3_avg = mean(water_level_sensor3, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = c(sensor2_avg, sensor2_corrected_avg, sensor3_avg), 
               names_to = "sensor_type", values_to = "water_level")

# **Time Series Plot: Daily Aggregated Water Levels**
ggplot(df_daily, aes(x = date_only, y = water_level, color = sensor_type, group = sensor_type)) +
  geom_line() +
  scale_color_manual(values = c("sensor2_avg" = "blue", 
                                "sensor2_corrected_avg" = "red", 
                                "sensor3_avg" = "green")) +
  labs(title = "Daily Time Series of Water Levels",
       x = "Date",
       y = "Water Level (m)",
       color = "Sensor Type") +
  theme_minimal() +
  theme(legend.position = "top")
##############################################################################################################################

ggplot(df, aes(x = date_only, y = water_level, color = sensor_type, group = sensor_type)) +
  geom_line() +
  scale_color_manual(values = c("sensor2_avg" = "blue", 
                                "sensor2_corrected_avg" = "red", 
                                "sensor3_avg" = "green")) +
  labs(title = "Daily Time Series of Water Levels",
       x = "Date",
       y = "Water Level (m)",
       color = "Sensor Type") +
  theme_minimal() +
  theme(legend.position = "top")
#
# Calculate residuals
df_overlap <- df_overlap %>%
  mutate(
    residual_uncorrected = water_level_sensor2 - water_level_sensor3,
    residual_predicted = predicted_water_level_sensor2 - water_level_sensor3
  )

# Create histogram of residuals
ggplot(df_overlap, aes(x = residual_uncorrected)) +
  geom_histogram(binwidth = 0.002, fill = "blue", alpha = 0.6, color = "black") +
  labs(title = "Histogram of Residuals: Uncorrected Sensor 2 vs. Sensor 3",
       x = "Residual (m)", y = "Count") +
  theme_minimal()

ggplot(df_overlap, aes(x = residual_predicted)) +
  geom_histogram(binwidth = 0.002, fill = "red", alpha = 0.6, color = "black") +
  labs(title = "Histogram of Residuals: Predicted Sensor 2 vs. Sensor 3",
       x = "Residual (m)", y = "Count") +
  theme_minimal()

