library(dplyr)

# Rename columns if not done yet
s1 <- s1 %>% rename(wtrlvl_s1 = wtrlvl)
s2 <- s2 %>% rename(wtrlvl_s2 = wtrlvl2)
s3 <- s3 %>% rename(wtrlvl_s3 = wtrlvl3)
s4 <- s4 %>% rename(wtrlvl_s4 = wtrlvl4)

# 1) Model s1 → s2
overlap_s1_s2 <- inner_join(s1, s2, by = "measurementTime") %>%
  filter(!is.na(wtrlvl_s1), !is.na(wtrlvl_s2))
model_s1_s2 <- lm(wtrlvl_s2 ~ wtrlvl_s1, data = overlap_s1_s2)

# 2) Model s3 → s2
overlap_s3_s2 <- inner_join(s3, s2, by = "measurementTime") %>%
  filter(!is.na(wtrlvl_s3), !is.na(wtrlvl_s2))
model_s3_s2 <- lm(wtrlvl_s2 ~ wtrlvl_s3, data = overlap_s3_s2)

# 3) Invert s3→s2 to approximate s2→s3
# For y = a + b*x, inverse is x = (y - a)/b
a <- coef(model_s3_s2)[1]
b <- coef(model_s3_s2)[2]

invert_s2_to_s3 <- function(s2_val) {
  (s2_val - a) / b
}

# 4) Model s3 → s4
overlap_s3_s4 <- inner_join(s3, s4, by = "measurementTime") %>%
  filter(!is.na(wtrlvl_s3), !is.na(wtrlvl_s4))
model_s3_s4 <- lm(wtrlvl_s4 ~ wtrlvl_s3, data = overlap_s3_s4)

# ------------------------
# Apply corrections

# s1 → s2 scale
s1 <- s1 %>%
  mutate(wtrlvl_s1_to_s2 = predict(model_s1_s2, newdata = data.frame(wtrlvl_s1 = wtrlvl_s1)))

# s1 → s3 scale by inverting s3→s2
s1 <- s1 %>%
  mutate(wtrlvl_s1_to_s3 = invert_s2_to_s3(wtrlvl_s1_to_s2))

# s1 → s4 scale via s3 → s4
s1 <- s1 %>%
  mutate(wtrlvl_s1_to_s4 = predict(model_s3_s4, newdata = data.frame(wtrlvl_s3 = wtrlvl_s1_to_s3)))

# ------------------------
# For sensor 3, also get s3 → s4 corrected directly (optional)
s3 <- s3 %>%
  mutate(wtrlvl_s3_to_s4 = predict(model_s3_s4, newdata = data.frame(wtrlvl_s3 = wtrlvl_s3)))

# ------------------------
# Final combined dataset on sensor 4 scale

s1_final <- s1 %>%
  select(measurementTime, wtrlvl_corrected = wtrlvl_s1_to_s4) %>%
  mutate(sensor = "s1_to_s4")

s3_final <- s3 %>%
  select(measurementTime, wtrlvl_corrected = wtrlvl_s3_to_s4) %>%
  mutate(sensor = "s3_to_s4")

s4_final <- s4 %>%
  select(measurementTime, wtrlvl_corrected = wtrlvl_s4) %>%
  mutate(sensor = "s4")

final_combined <- bind_rows(s1_final, s3_final, s4_final) %>%
  arrange(measurementTime) %>%
  group_by(measurementTime) %>%
  arrange(factor(sensor, levels = c("s4", "s3_to_s4", "s1_to_s4"))) %>%
  slice(1) %>%
  ungroup()

# ------------------------
# Plot final corrected timeseries

library(ggplot2)
ggplot(final_combined, aes(x = measurementTime, y = wtrlvl_corrected, color = sensor)) +
  geom_line() +
  labs(
    title = "Corrected Water Level Timeseries (Sensor 1, 3 corrected to Sensor 4 scale)",
    x = "Time",
    y = "Water Level (corrected)"
  ) +
  theme_minimal()
