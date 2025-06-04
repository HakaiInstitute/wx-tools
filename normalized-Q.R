library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)

# Example input data (replace this with your actual dataset)
data <- data.frame(
  month = c(
    "2014-09-10 15:40:00", "2014-09-10 15:45:00", "2014-10-10 15:50:00",
    "2014-10-31 23:55:00", "2014-11-30 23:55:00", "2014-12-31 23:55:00"
  ),
  discharge = c(3.75, 7.5, 11.25, 15, 18.75, 22.5),
  rain = c(NA, NA, 50, 60, 70, 80)
)

# Ensure `month` column is a proper datetime
data <- df %>%
  mutate(date = as.POSIXct(date, format = "%Y-%m-%d %H:%M"))

# Calculate the last value for each month
last_values <- data %>%
  mutate(year_month = floor_date(date, "month")) %>%  # Create year-month grouping
  group_by(year_month) %>%  # Group by year and month
  filter(date == max(date)) %>%  # Keep only the last row for each group
  ungroup()

# Extract year and month
last_values <- last_values %>%
  mutate(
    year = year(date),  # Extract year from the `date` column
    month_label = format(date, "%b")  # Extract readable month name using format()
  )

# Calculate the last value for each month
last_values <- data %>%
  mutate(year_month = floor_date(date, "month")) %>%  # Create year-month grouping
  group_by(year_month) %>%  # Group by year and month
  filter(date == max(date)) %>%  # Keep only the last row for each group
  ungroup()

# Extract year and month
last_values <- last_values %>%
  mutate(
    year = year(date),  # Extract year from the `date` column
    month_label = format(date, "%b")  # Extract readable month name using format()
  )

# Normalize discharge with watershed area in km²
watershed_area_km2 <- 5.707137
last_values <- last_values %>%
  mutate(
    normalized_discharge = (discharge / (watershed_area_km2 * 1e6)) * 1e3  # Convert m³ to mm
  )

# Pivot longer for plotting
data_long <- last_values %>%
  select(year, month_label, normalized_discharge, rain) %>%
  pivot_longer(cols = c(normalized_discharge, rain), 
               names_to = "variable", values_to = "value")

# Example plot code with facet_wrap to split into two pages
ggplot(data_long, aes(x = month_label, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(
    values = c("normalized_discharge" = "red", "rain" = "blue"),
    labels = c("Normalized Discharge", "Rain")
  ) +
  facet_wrap(~ year, ncol = 2, scales = "free_y") +  # Change ncol to split across two columns
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color = "black"),
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(color = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # Rotate x-axis labels for readability
    axis.text.y = element_text(size = 12),  # Increase y-axis text size
    axis.title = element_text(size = 14),  # Increase axis title size
    strip.text.x = element_text(size = 14),  # Increase facet labels font size
    panel.spacing = unit(1, "lines")  # Increase space between facets
  ) +
  labs(
    x = "Month",
    y = "Value (mm)",
    fill = "Variable",
    title = "Normalized Discharge and Rain by Year"
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),  # Increase plot title size
    legend.position = "top"  # Position the legend at the top to avoid overlap
  )
ggsave("normalized_discharge_844V5.pdf", width = 12, height = 8, dpi = 300)
