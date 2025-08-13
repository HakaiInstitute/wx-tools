# --- Load required libraries ---
library(tidyverse)
library(lubridate)
library(broom)
library(Metrics)
library(changepoint)
library(zoo)

# --- Step 1: Clean function ---
clean_sensor_data <- function(sensor_df) {
  sensor_df %>%
    filter(!is.na(stage)) %>%
    filter(stage > 0, stage < 20) %>%  # Adjust range as needed
    mutate(timestamp = ymd_hms(timestamp)) %>%
    arrange(timestamp)
}

# --- Step 2: Clean your data ---
sensor1_clean <- clean_sensor_data(sensor1)
sensor2_clean <- clean_sensor_data(sensor2)
sensor3_clean <- clean_sensor_data(sensor3)
sensor4_clean <- clean_sensor_data(sensor4)

# --- Step 3: Comparison + model selection + adjustment ---
compare_and_adjust <- function(df_old, df_new, label_old, label_new) {
  # Merge datasets
  merged <- inner_join(df_old, df_new, by = "timestamp", suffix = c("_old", "_new")) %>%
    drop_na()
  
  if (nrow(merged) < 10) {
    message("Not enough overlap between ", label_old, " and ", label_new)
    return(NULL)
  }
  
  # Fit models
  linear_mod <- lm(stage_new ~ stage_old, data = merged)
  quad_mod   <- lm(stage_new ~ stage_old + I(stage_old^2), data = merged)
  loess_mod  <- loess(stage_new ~ stage_old, data = merged, span = 0.75)
  
  # Predict and residuals
  merged <- merged %>%
    mutate(
      pred_linear = predict(linear_mod),
      pred_quad   = predict(quad_mod),
      pred_loess  = predict(loess_mod),
      resid_linear = stage_new - pred_linear,
      resid_quad   = stage_new - pred_quad,
      resid_loess  = stage_new - pred_loess
    ) %>%
    arrange(timestamp) %>%
    mutate(resid_roll_3d = zoo::rollmean(resid_loess, k = 96, fill = NA))  # ~3-day roll mean
  
  # Change points
  cpt <- changepoint::cpt.mean(na.omit(merged$resid_loess), method = "PELT", penalty = "MBIC")
  change_times <- merged$timestamp[changepoint::cpts(cpt)]
  
  # Metrics
  metrics <- tibble(
    Model = c("Linear", "Quadratic", "LOESS"),
    RMSE = c(
      rmse(merged$stage_new, merged$pred_linear),
      rmse(merged$stage_new, merged$pred_quad),
      rmse(merged$stage_new, merged$pred_loess)
    ),
    R_squared = c(
      summary(linear_mod)$r.squared,
      summary(quad_mod)$r.squared,
      NA
    ),
    AIC = c(
      AIC(linear_mod),
      AIC(quad_mod),
      NA
    )
  ) %>%
    mutate(Comparison = paste(label_old, "vs", label_new))
  
  # Choose best model by RMSE
  best_model_name <- metrics %>% slice_min(RMSE) %>% pull(Model)
  best_model <- switch(
    best_model_name,
    "Linear" = linear_mod,
    "Quadratic" = quad_mod,
    "LOESS" = loess_mod
  )
  
  # Apply adjustment to full historical sensor
  adjusted <- df_old %>%
    mutate(
      adjusted_stage = predict(best_model, newdata = tibble(stage_old = stage))
    ) %>%
    rename(original_stage = stage)
  
  # --- Plots ---
  p1 <- ggplot(merged, aes(x = stage_old, y = stage_new)) +
    geom_point(alpha = 0.5, size = 1) +
    geom_line(aes(y = pred_linear), color = "blue") +
    geom_line(aes(y = pred_quad), color = "purple", linetype = "dashed") +
    geom_line(aes(y = pred_loess), color = "green", linetype = "dotdash") +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dotted") +
    labs(title = paste(label_old, "vs", label_new, "- Model Fit"))
  
  p2 <- ggplot(merged, aes(x = timestamp)) +
    geom_line(aes(y = resid_loess), color = "darkorange", alpha = 0.5) +
    geom_line(aes(y = resid_roll_3d), color = "black") +
    geom_vline(xintercept = as.numeric(change_times), color = "red", linetype = "dotted") +
    labs(title = "LOESS Residuals Over Time", subtitle = "Change points (red)")
  
  print(p1)
  print(p2)
  print(metrics)
  
  # Return everything useful
  return(list(
    metrics = metrics,
    adjusted = adjusted,
    best_model = best_model,
    change_points = change_times
  ))
}

# --- Step 4: Run comparisons and adjustments ---
result_1 <- compare_and_adjust(sensor1_clean, sensor4_clean, "Sensor 1", "Sensor 4")
result_2 <- compare_and_adjust(sensor2_clean, sensor4_clean, "Sensor 2", "Sensor 4")
result_3 <- compare_and_adjust(sensor3_clean, sensor4_clean, "Sensor 3", "Sensor 4")

# --- Step 5: Export results ---

# 📤 Combine metrics and export
all_metrics <- bind_rows(
  result_1$metrics,
  result_2$metrics,
  result_3$metrics
)

write_csv(all_metrics, "sensor_model_metrics_summary.csv")

# 📤 Export adjusted time series
write_csv(result_1$adjusted, "sensor1_adjusted_to_sensor4.csv")
write_csv(result_2$adjusted, "sensor2_adjusted_to_sensor4.csv")
write_csv(result_3$adjusted, "sensor3_adjusted_to_sensor4.csv")
