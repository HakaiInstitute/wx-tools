library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(plotly)
library(Metrics)

# Load data
df <- read.csv("ssnkoeye_pt.csv", header = FALSE, skip = 4, stringsAsFactors = FALSE)

# Load column headers and apply them
fileheaders <- read.csv("ssnkoeye_pt.csv", nrows = 1, as.is = TRUE, header = FALSE)
colnames(df) <- fileheaders


# Step 1: Elevation correction for Sensor 2
df <- df %>%
  mutate(sensor_2_elevation_corrected = water_level_sensor2 - 0.135,
         year = year(timestamp))  # Extract year

# Step 2: Identify overlap (non-NA values for both sensors) non elevation corrected
df_overlap <- df %>%
  filter(!is.na(water_level_sensor2) & !is.na(water_level_sensor3))

# Step 3: Build regression model (ONLY on overlapping data) on non elevation corrected
model_predicted <- lm(water_level_sensor3 ~ water_level_sensor2, data = df_overlap)

# Step 4: Apply prediction only after model exists
df_overlap <- df_overlap %>%
  mutate(sensor_2_predicted = predict(model_predicted, newdata = df_overlap))

# Step 5: Build uncorrected model (to compare)
model_uncorrected <- lm(water_level_sensor3 ~ water_level_sensor2, data = df_overlap)

# Step 6: Compute residuals
df_overlap <- df_overlap %>%
  mutate(residuals_uncorrected = water_level_sensor3 - predict(model_uncorrected, newdata = df_overlap),
         residuals_predicted = water_level_sensor3 - sensor_2_predicted)

# Step 7: Calculate RMSE and R²
rmse_uncorrected <- rmse(df_overlap$water_level_sensor3, predict(model_uncorrected, newdata = df_overlap))
r2_uncorrected <- summary(model_uncorrected)$r.squared

rmse_predicted <- rmse(df_overlap$water_level_sensor3, df_overlap$sensor_2_predicted)
r2_predicted <- summary(model_predicted)$r.squared

# Step 8: Residuals plot for uncorrected Sensor 2 vs. Sensor 3
p1 <- ggplot(df_overlap, aes(x = water_level_sensor2, y = residuals_uncorrected)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals: Uncorrected Sensor 2 vs. Sensor 3",
       x = "Water Level Sensor 2 (Uncorrected)",
       y = "Residuals") +
  annotate("text", x = min(df_overlap$water_level_sensor2, na.rm = TRUE), 
           y = max(df_overlap$residuals_uncorrected, na.rm = TRUE), 
           label = paste("RMSE:", round(rmse_uncorrected, 3), "\nR²:", round(r2_uncorrected, 3)), 
           hjust = 0, size = 5, color = "black") +
  theme_minimal()

# Step 9: Residuals plot for predicted Sensor 2 vs. Sensor 3
p2 <- ggplot(df_overlap, aes(x = sensor_2_predicted, y = residuals_predicted)) +
  geom_point(alpha = 0.5, color = "green") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals: Predicted Sensor 2 vs. Sensor 3",
       x = "Predicted Water Level Sensor 2",
       y = "Residuals") +
  annotate("text", x = min(df_overlap$sensor_2_predicted, na.rm = TRUE), 
           y = max(df_overlap$residuals_predicted, na.rm = TRUE), 
           label = paste("RMSE:", round(rmse_predicted, 3), "\nR²:", round(r2_predicted, 3)), 
           hjust = 0, size = 5, color = "black") +
  theme_minimal()

# Step 10: Convert to interactive plots
ggplotly(p1)
ggplotly(p2)
