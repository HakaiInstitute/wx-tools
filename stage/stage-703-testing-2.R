library(dplyr)
library(ggplot2)
library(purrr)
library(rlang)
library(patchwork)
library(tidyr)

# --- Define sensor pairs and labels ---
pairs <- list(
  list(df1 = s1, col1 = "wtrlvl",  df2 = s2, col2 = "wtrlvl2", label = "s1 - s2"),
  list(df1 = s2, col1 = "wtrlvl2", df2 = s3, col2 = "wtrlvl3", label = "s2 - s3"),
  list(df1 = s3, col1 = "wtrlvl3", df2 = s4, col2 = "wtrlvl4", label = "s3 - s4")
)

# --- 1. Calculate residuals for all pairs ---
residuals_all <- map_dfr(pairs, function(p) {
  inner_join(p$df1, p$df2, by = "measurementTime") %>%
    filter(!is.na(.data[[p$col1]]), !is.na(.data[[p$col2]])) %>%
    mutate(residual = .data[[p$col1]] - .data[[p$col2]],
           pair = p$label) %>%
    select(measurementTime, residual, pair)
})

# Plot residuals over time for all pairs  #what is older sensor here?
ggplot(residuals_all, aes(x = measurementTime, y = residual, color = pair)) +
  geom_line(alpha = 0.7) +
  labs(title = "Residuals Over Time for Each Sensor Pair",
       x = "Time", y = "Residual (Older Sensor - Newer Sensor)",
       color = "Sensor Pair") +
  theme_minimal()

# --- 2. Fit linear models for each sensor overlap pair and summarize ---
results <- lapply(pairs, function(p) {
  overlap <- inner_join(p$df1, p$df2, by = "measurementTime")
  mod <- lm(overlap[[p$col2]] ~ overlap[[p$col1]])
  sm <- summary(mod)
  
  stats <- data.frame(
    pair = paste(p$col1, "→", p$col2),
    slope = coef(mod)[2],
    intercept = coef(mod)[1],
    r2 = sm$r.squared,
    n = nrow(overlap)
  )
  
  plot <- ggplot(overlap, aes(x = !!sym(p$col1), y = !!sym(p$col2))) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", color = "red") +
    labs(title = paste(p$col1, "vs", p$col2),
         subtitle = paste0("y = ", round(coef(mod)[1], 4), " + ", round(coef(mod)[2], 4), "x | R² = ", round(sm$r.squared, 4)),
         x = p$col1,
         y = p$col2) +
    theme_minimal()
  
  list(stats = stats, model = mod, plot = plot)
})

results_df <- do.call(rbind, lapply(results, `[[`, "stats"))
print(results_df)

# Print scatter plots for each pair
lapply(results, function(x) print(x$plot))

# --- 3. Apply chained corrections to align to sensor 4 scale ---
# --- Step 1: Fit overlap models ---

# s1 → s2
m12 <- s1 %>%
  inner_join(s2, by = "measurementTime") %>%
  lm(wtrlvl2 ~ wtrlvl, data = .)

# s2 → s3
m23 <- s2 %>%
  inner_join(s3, by = "measurementTime") %>%
  lm(wtrlvl3 ~ wtrlvl2, data = .)

overlap_s2_s3 <- inner_join(s2, s3, by = "measurementTime")
nrow(overlap_s2_s3)
summary(overlap_s2_s3)


# s3 → s4
m34 <- s3 %>%
  inner_join(s4, by = "measurementTime") %>%
  lm(wtrlvl4 ~ wtrlvl3, data = .)

# --- Step 2: Apply transformations to get everything into s4's scale ---

# s3 adjusted to s4 scale
s3_adj <- s3 %>%
  mutate(wtrlvl4_adj = predict(m34, newdata = .))

# s2 adjusted to s4 scale
s2_adj <- s2 %>%
  mutate(wtrlvl4_adj = predict(m23, newdata = .) ) %>%      # s2 → s3 scale
  mutate(wtrlvl4_adj = predict(m34, newdata = data.frame(wtrlvl3 = wtrlvl4_adj))) # then s3 → s4 scale

# s1 adjusted to s4 scale
s1_adj <- s1 %>%
  mutate(wtrlvl4_adj = predict(m12, newdata = .)) %>%       # s1 → s2 scale
  mutate(wtrlvl4_adj = predict(m23, newdata = data.frame(wtrlvl2 = wtrlvl4_adj))) %>% # s2 → s3 scale
  mutate(wtrlvl4_adj = predict(m34, newdata = data.frame(wtrlvl3 = wtrlvl4_adj)))     # s3 → s4 scale

# --- Step 3: Combine everything into one time series ---

combined <- bind_rows(
  s1_adj %>% select(measurementTime, wtrlvl4_adj),
  s2_adj %>% select(measurementTime, wtrlvl4_adj),
  s3_adj %>% select(measurementTime, wtrlvl4_adj),
  s4 %>% rename(wtrlvl4_adj = wtrlvl4) %>% select(measurementTime, wtrlvl4_adj)
) %>%
  arrange(measurementTime) %>%
  distinct(measurementTime, .keep_all = TRUE)  # keep earliest entry for duplicates
# --- 5. Plot original vs adjusted timeseries for each sensor ---

plot_sensor_comparison <- function(df, original_col, adjusted_col, sensor_name) {
  df_long <- df %>%
    select(measurementTime, original = !!sym(original_col), adjusted = !!sym(adjusted_col)) %>%
    pivot_longer(cols = c(original, adjusted), names_to = "type", values_to = "water_level")
  
  ggplot(df_long, aes(x = measurementTime, y = water_level, color = type)) +
    geom_line(alpha = 0.8) +
    labs(title = paste(sensor_name, ": Original vs Adjusted Water Level"),
         x = "Time", y = "Water Level (corrected units)") +
    theme_minimal()
}

plot_sensor_comparison(s1_adj, "wtrlvl", "wtrlvl4_adj", "Sensor 1")
plot_sensor_comparison(s2_adj, "wtrlvl2", "wtrlvl4_adj", "Sensor 2")
plot_sensor_comparison(s3_adj, "wtrlvl3", "wtrlvl4_adj", "Sensor 3")

# (Sensor 4 adjusted = original, no need to plot)

# --- 6. Optional: Residual diagnostics (histograms, residuals vs fitted) for each pair ---
plots_residuals <- lapply(results, function(res) {
  mod <- res$model
  data <- data.frame(
    fitted = fitted(mod),
    residuals = resid(mod)
  )
  
  p1 <- ggplot(data, aes(x = fitted, y = residuals)) +
    geom_point(alpha = 0.2, size = 0.7) +
    geom_hline(yintercept = 0, color = "red") +
    labs(title = "Residuals vs Fitted", x = "Fitted values", y = "Residuals") +
    theme_minimal()
  
  p2 <- ggplot(data, aes(x = residuals)) +
    geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7) +
    labs(title = "Residual Histogram", x = "Residuals", y = "Count") +
    theme_minimal()
  
  p1 + p2
})

print(wrap_plots(plots_residuals, ncol = 1))
#################################################################################################################################
#final timeseries
################################################################################################################################
library(dplyr)

# Select only relevant columns and mark sensor source
s1_sel <- s1_adj %>% select(measurementTime, corrected = wtrlvl4_adj) %>% mutate(sensor = "s1")
s2_sel <- s2_adj %>% select(measurementTime, corrected = wtrlvl4_adj) %>% mutate(sensor = "s2")
s3_sel <- s3_adj %>% select(measurementTime, corrected = wtrlvl4_adj) %>% mutate(sensor = "s3")
s4_sel <- s4 %>% select(measurementTime, corrected = wtrlvl4) %>% mutate(sensor = "s4")

# Combine all sensors into one dataframe
combined <- bind_rows(s4_sel, s3_sel, s2_sel, s1_sel) %>%
  arrange(measurementTime) %>%
  group_by(measurementTime) %>%
  slice(1) %>%      # Take the first (highest priority) sensor value at each time
  ungroup()

# Final unified corrected stage timeseries
corrected_unified <- combined %>%
  select(measurementTime, corrected, sensor)

# View a summary
head(corrected_unified)
summary(corrected_unified$corrected)
table(corrected_unified$sensor)  # Count how many points come from each sensor

#####################################################################################################################################
#graphing
######################################################################################################################################
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(plotly)

# Prepare original data combined with sensor ID
original_combined <- bind_rows(
  s1 %>% select(measurementTime, water_level = wtrlvl) %>% mutate(sensor = "s1"),
  s2 %>% select(measurementTime, water_level = wtrlvl2) %>% mutate(sensor = "s2"),
  s3 %>% select(measurementTime, water_level = wtrlvl3) %>% mutate(sensor = "s3"),
  s4 %>% select(measurementTime, water_level = wtrlvl4) %>% mutate(sensor = "s4")
)

# Plot 1: Corrected unified timeseries
# Make sure your data is sorted by time and sensor before plotting:
corrected_unified <- corrected_unified %>%
  arrange(sensor, measurementTime)

original_combined <- original_combined %>%
  arrange(sensor, measurementTime)

# Then in ggplot, specify group = sensor:
p1 <- ggplot(corrected_unified, aes(
  x = measurementTime, y = corrected,
  color = sensor, group = sensor,
  text = paste("Time:", measurementTime, "<br>Value:", round(corrected, 3), "<br>Sensor:", sensor)
)) +
  geom_line(alpha = 0.8) +
  labs(
    title = "Unified Corrected Water Level Timeseries (Scaled to Sensor 4)",
    x = "Time", y = "Corrected Water Level", color = "Source Sensor"
  ) +
  theme_minimal()

p2 <- ggplot(original_combined, aes(
  x = measurementTime, y = water_level,
  color = sensor, group = sensor,
  text = paste("Time:", measurementTime, "<br>Value:", round(water_level, 3), "<br>Sensor:", sensor)
)) +
  geom_line(alpha = 0.6) +
  labs(
    title = "Original Sensor Water Level Timeseries",
    x = "Time", y = "Original Water Level", color = "Sensor"
  ) +
  theme_minimal()
p1
