#703 stage qc

#wtrlvl 2014- to 208-09-11 22:15
#wtrlvl2 2017-11-13 15:40 to 20201-03-25
#wtrlvl3 2018-09-14 21:15 to 2023-08-03
#wtrlvl4 2021-09-04 to present

# Summary of Steps
# 
# Visual and site analysis: Look for signs of channel sedimentation or erosion.
# 
# Stage comparison: Use overlap data to compare the stage between sensors.
# 
# Stage-discharge analysis: Compare stage vs. discharge to check for shifts.
# 
# Use hydraulic modeling (if needed) to confirm hydraulic changes.
# 
# Assess seasonality: Check if seasonal or temporal effects complicate the relationship.

library(dplyr)
library(ggplot2)

# Example assumes each dataframe has:
# measurementTime (POSIXct), wtrlvl (or wtrlvl2, wtrlvl3, wtrlvl4)
# Rename water level columns for consistency before processing:

s1 <- s1 %>% rename(wtrlvl_s1 = wtrlvl)
s2 <- s2 %>% rename(wtrlvl_s2 = wtrlvl2)
s3 <- s3 %>% rename(wtrlvl_s3 = wtrlvl3)
s4 <- s4 %>% rename(wtrlvl_s4 = wtrlvl4)

# 1) Model s1 -> s2 (overlap s1 and s2)
overlap_s1_s2 <- inner_join(s1, s2, by = "measurementTime") %>%
  filter(!is.na(wtrlvl_s1), !is.na(wtrlvl_s2))
model_s1_s2 <- lm(wtrlvl_s2 ~ wtrlvl_s1, data = overlap_s1_s2)
summary(model_s1_s2)
plot(model_s1_s2)

# 2) Model s2 -> s3 (overlap s2 and s3)
overlap_s2_s3 <- inner_join(s2, s3, by = "measurementTime") %>%
  filter(!is.na(wtrlvl_s2), !is.na(wtrlvl_s3))
model_s2_s3 <- lm(wtrlvl_s3 ~ wtrlvl_s2, data = overlap_s2_s3)
summary(model_s2_s3)
plot(model_s2_s3)

# 3) Model s3 -> s4 (overlap s3 and s4)
overlap_s3_s4 <- inner_join(s3, s4, by = "measurementTime") %>%
  filter(!is.na(wtrlvl_s3), !is.na(wtrlvl_s4))
model_s3_s4 <- lm(wtrlvl_s4 ~ wtrlvl_s3, data = overlap_s3_s4)
summary(model_s3_s4)
plot(model_s3_s4)

library(ggplot2)
library(gridExtra)  # to arrange multiple plots

# Helper function to create diagnostic plots for a model and data
plot_diagnostics <- function(model, data, xvar_name) {
  # Extract residuals and fitted values
  res <- residuals(model)
  fitted <- fitted(model)
  
  # Add residuals and fitted to data
  df <- data.frame(
    residuals = res,
    fitted = fitted,
    measurementTime = data$measurementTime
  )
  
  # 1. Residuals vs Fitted
  p1 <- ggplot(df, aes(x = fitted, y = residuals)) +
    geom_point(alpha = 0.3, size = 0.7) +
    geom_hline(yintercept = 0, color = "red") +
    labs(title = paste("Residuals vs Fitted:", xvar_name),
         x = "Fitted values", y = "Residuals") +
    theme_minimal()
  
  # 2. Histogram of residuals
  p2 <- ggplot(df, aes(x = residuals)) +
    geom_histogram(bins = 50, fill = "skyblue", color = "black") +
    labs(title = paste("Histogram of residuals:", xvar_name),
         x = "Residuals", y = "Count") +
    theme_minimal()
  
  # 3. Residuals over time
  p3 <- ggplot(df, aes(x = measurementTime, y = residuals)) +
    geom_point(alpha = 0.3, size = 0.7) +
    geom_hline(yintercept = 0, color = "red") +
    labs(title = paste("Residuals over Time:", xvar_name),
         x = "Time", y = "Residuals") +
    theme_minimal()
  
  list(p1, p2, p3)
}

# Create diagnostics for each model
plots_s1_s2 <- plot_diagnostics(model_s1_s2, overlap_s1_s2, "s1 -> s2")
plots_s2_s3 <- plot_diagnostics(model_s2_s3, overlap_s2_s3, "s2 -> s3")
plots_s3_s4 <- plot_diagnostics(model_s3_s4, overlap_s3_s4, "s3 -> s4")

# Arrange and display plots per model (3 plots per model)
grid.arrange(grobs = plots_s1_s2, ncol = 3, top = "Diagnostics for s1 -> s2")
grid.arrange(grobs = plots_s2_s3, ncol = 3, top = "Diagnostics for s2 -> s3")
grid.arrange(grobs = plots_s3_s4, ncol = 3, top = "Diagnostics for s3 -> s4")

# ----------------------------
# Apply chained corrections

# Correct s1 to s2 scale
s1 <- s1 %>%
  mutate(wtrlvl_s1_to_s2 = predict(model_s1_s2, newdata = data.frame(wtrlvl_s1 = wtrlvl_s1)))

# Correct s1_to_s2 to s3 scale
s1 <- s1 %>%
  mutate(wtrlvl_s1_to_s3 = predict(model_s2_s3, newdata = data.frame(wtrlvl_s2 = wtrlvl_s1_to_s2)))

# Correct s1_to_s3 to s4 scale (final correction)
s1 <- s1 %>%
  mutate(wtrlvl_s1_to_s4 = predict(model_s3_s4, newdata = data.frame(wtrlvl_s3 = wtrlvl_s1_to_s3)))

# For s2, correct directly to s3, then to s4
s2 <- s2 %>%
  mutate(wtrlvl_s2_to_s3 = predict(model_s2_s3, newdata = data.frame(wtrlvl_s2 = wtrlvl_s2)),
         wtrlvl_s2_to_s4 = predict(model_s3_s4, newdata = data.frame(wtrlvl_s3 = wtrlvl_s2_to_s3)))

# For s3, correct directly to s4
s3 <- s3 %>%
  mutate(wtrlvl_s3_to_s4 = predict(model_s3_s4, newdata = data.frame(wtrlvl_s3 = wtrlvl_s3)))

# For s4, no correction needed
s4 <- s4 %>%
  mutate(wtrlvl_s4_corrected = wtrlvl_s4)

# ----------------------------
# Combine corrected data

combined <- bind_rows(
  s1 %>% select(measurementTime, wtrlvl_corrected = wtrlvl_s1_to_s4) %>% mutate(sensor = "s1"),
  s2 %>% select(measurementTime, wtrlvl_corrected = wtrlvl_s2_to_s4) %>% mutate(sensor = "s2"),
  s3 %>% select(measurementTime, wtrlvl_corrected = wtrlvl_s3_to_s4) %>% mutate(sensor = "s3"),
  s4 %>% select(measurementTime, wtrlvl_corrected = wtrlvl_s4_corrected) %>% mutate(sensor = "s4")
) %>%
  arrange(measurementTime)

# Remove duplicate timestamps prioritizing sensor 4 > 3 > 2 > 1
priority <- c("s4", "s3", "s2", "s1")
combined <- combined %>%
  group_by(measurementTime) %>%
  arrange(factor(sensor, levels = priority)) %>%
  slice(1) %>%
  ungroup()

# ----------------------------
# Plot combined corrected timeseries
ggplot(combined, aes(measurementTime, wtrlvl_corrected, color = sensor)) +
  geom_line() +
  labs(title = "Chained Corrected Water Level Time Series",
       x = "Time", y = "Water Level (corrected)") +
  theme_minimal()