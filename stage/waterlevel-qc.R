library(tidyverse)
library(zoo)

# Sample data frame
# df <- data.frame(timestamp = seq.POSIXt(from = as.POSIXct("2024-08-01 00:00"), 
#                                         to = as.POSIXct("2024-08-01 23:55"), by = "5 min"),
#                 watlev = c(runif(288, min = 0.5, max = 1.5)))

# 1. Fill any missing gaps with linear interpolation
df <- df %>%
  complete(timestamp = seq(min(timestamp), max(timestamp), by = "5 min")) %>%
  arrange(timestamp) %>%
  mutate(watlev_filled = na.approx(watlev, na.rm = FALSE))

# 2. Calculate the rolling mean of water level changes over a 240-minute window (48 periods of 5 minutes)
df <- df %>%
  mutate(rolling_mean = rollmean(watlev_filled, k = 48, fill = NA, align = "center"),
         change = watlev_filled - rolling_mean)

# 3. Detect spikes where the absolute change in water level is >= 0.001 within the 240-minute window
df <- df %>%
  mutate(is_spike = ifelse(abs(change) >= 0.001, TRUE, FALSE))

#4. Perform linear interpolation to fill gaps caused by spikes
spike_indices <- which(df$is_spike)

# Perform linear interpolation to fill gaps caused by spikes
df <- df %>%
  mutate(watlev_corrected = ifelse(is_spike, NA, watlev_filled)) %>%
  arrange(timestamp) %>%
  mutate(watlev_corrected = zoo::na.approx(watlev_corrected, na.rm = FALSE))
#5.flag corrected points
df <- df %>%
mutate(flag = ifelse(is_spike, "corrected", NA))



# 5. Create a summary graph showing before and after changes
ggplot(df, aes(x = timestamp)) +
  geom_line(aes(y = watlev_filled), color = "blue", size = 1, alpha = 0.5) +
  geom_line(aes(y = watlev_corrected), color = "red", size = 1, alpha = 0.7) +
 # geom_point(data = df %>% filter(!is.na(flag)), 
            # aes(y = watlev_corrected), color = "red", size = 2) +
  labs(title = "Water Level Time Series Before and After Spike Correction",
       x = "Timestamp", y = "Water Level") +
  theme_minimal()

#6. final dataframe
df1 <- df %>%
  mutate(values = ifelse(is.na(watlev_corrected), watlev_filled, watlev_corrected),
         flag = ifelse(is_spike, "EV: Blockage, corrected via linear interpolation: QC'd by EH", "AV: QC'd by EH")) %>% 
  select(timestamp, values, flag)
write.csv(df1, "SSN626US-20240708-corr.csv")
