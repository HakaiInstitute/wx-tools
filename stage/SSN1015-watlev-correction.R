library(tidyverse)
library(zoo)

# Assume df is your data frame with 'timestamp' and 'watlev' columns
# Convert timestamp to POSIXct if it isn't already
df <- df %>%
  mutate(timestamp = as.POSIXct(timestamp))

# Fill gaps using linear interpolation first (you can modify this to spline if needed)
df <- df %>%
  complete(timestamp = seq(min(timestamp), max(timestamp), by = "5 min")) %>%
  mutate(watlev = zoo::na.approx(watlev, x = timestamp, na.rm = FALSE))

# Detect spikes: Calculate the difference over a 30-minute window
df <- df %>%
  arrange(timestamp) %>%
  mutate(
    diff_30min = rollapply(watlev, width = 6, FUN = function(x) max(x) - min(x), fill = NA, align = "right")
  )

# Remove data where the difference exceeds 0.001
df_cleaned <- df %>%
  filter(is.na(diff_30min) | abs(diff_30min) <= 0.001) %>%
  select(-diff_30min)

# Fill gaps caused by spike removal using spline interpolation
df_filled <- df_cleaned %>%
  mutate(watlev = zoo::na.spline(watlev, x = timestamp))

# Optional: Reorder the columns back to original if needed
df_filled <- df_filled %>%
  arrange(timestamp)

# View the resulting data frame
print(df_filled)

df_filled <- df_filled %>%
  mutate(
    altered = is.na(watlev) | !timestamp %in% df$timestamp
  )

p1 <- ggplot(df, aes(x = timestamp, y = watlev)) +
  geom_line(color = "blue") +
  ggtitle("Original Time Series") +
  xlab("Timestamp") +
  ylab("Water Level") +
  theme_minimal()

# Cleaned Time Series Plot with color-coded altered values
p2 <- ggplot(df_filled, aes(x = timestamp, y = watlev)) +
  geom_line(aes(color = altered)) +
  geom_point(aes(color = altered), size = 0.5) +
  scale_color_manual(values = c("black", "red"), labels = c("Original", "Altered")) +
  ggtitle("Cleaned Time Series with Altered Values") +
  xlab("Timestamp") +
  ylab("Water Level") +
  theme_minimal() +
  labs(color = "Data Status")

# Arrange the plots side by side
grid.arrange(p1, p2, ncol = 2)
