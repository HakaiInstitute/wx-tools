library(ggplot2)
library(dplyr)
library(zoo)

data$timestamp <- as.POSIXct(data$timestamp, format="%Y-%m-%d %H:%M:%S", tz="UTC")

data <- data %>%
  mutate(date = as.Date(timestamp),
         temp_diff = c(NA, diff(data$salm_trib5_t1_21729401)),  # Temperature change between readings
         roll_range = rollapply(data$salm_trib5_t1_21729401, width = 24, FUN = function(x) max(x) - min(x), fill = NA, align = "right"),
         dewatering_flag = roll_range >5)  # Threshold variation of 5°C

# Extend flagging to capture full dewatering periods
data <- data %>%
  mutate(dewatering_group = cumsum(c(0, diff(dewatering_flag)) == 1)) %>%
  group_by(dewatering_group) %>%
  mutate(dewatering_event = any(dewatering_flag)) %>%
  ungroup()

# Plot temperature and detected events
ggplot(data, aes(x = timestamp, y = salm_trib5_t1_21729401)) +
  geom_line(color = "blue") +
  geom_point(data = dewatering_events, aes(x = timestamp, y = salm_trib5_t1_21729401), color = "red", size = 2) +
  labs(title = "Stream Temperature with Dewatering Events Highlighted",
       x = "DateTime",
       y = "Temperature (°C)") +
  theme_minimal()
str(dewatering_events)


