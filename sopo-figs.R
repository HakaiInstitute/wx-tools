###WSC dataframes
# Install and load necessary packages
if (!requireNamespace("tidyhydat", quietly = TRUE)) {
  install.packages("tidyhydat")
}
library(tidyhydat)
library(dplyr)
library(scales)

download_hydat()

#08HF015 Eve below Kunnum HGH
#08HD007 Salmon above Memekay HGH
#08HD006 Salmon near Sayward HGH TW
#08HF004 Tsitika below Catharine HGH TW

# Define the number of seconds in a year
seconds_per_year <- 31536000  # 60 * 60 * 24 * 365

fraser<-hy_daily_flows(station_number = "08MF005", 
               start_date = "1920-01-01", 
               end_date = "2025-01-01")

# Convert m³/s to total discharge volume (m³) per year
fraser$discharge_m3 <- fraser$Value * seconds_per_year

# Fit a linear trend model
linear_model <- lm(fraser$discharge_m3 ~ Date, data = fraser)
fraser$trend <- predict(linear_model)

# Calculate discharge anomalies (m³)
fraser$anomaly <- fraser$discharge_m3 - fraser$trend

# Ensure Date column is properly formatted (if needed)
fraser$Year <- format(fraser$Date, "%Y")  # Extract year if Date is a full date

# Function to format y-axis labels as 10^x notation
power10_format <- function(x) {
  parse(text = gsub("e\\+?", " %*% 10^", scientific_format()(x)))  # Converts "1e+09" to "1 × 10^9"
}

# Extract the slope (rate of increase) in m³/year
slope_m3_per_year <- coef(linear_model)[2]  # slope is the 2nd coefficient

# Convert slope to km³/year (1 km³ = 1e9 m³)
slope_km3_per_year <- slope_m3_per_year / 1e9

# Print the rate of increase in km³/year
cat("Rate of increase in discharge: ", slope_km3_per_year, "km³/year\n")

# Create the anomaly bar graph
ggplot(fraser, aes(x = factor(year), y = discharge_m3 - predict(linear_model), fill = (discharge_m3 - predict(linear_model)) > 0)) +
  geom_bar(stat = "identity", color = "black", width = 0.6) +  # Bars for anomalies
  geom_line(aes(x = year, y = predict(linear_model)), color = "black", size = 1) +  # Trend line
  scale_fill_manual(values = c("blue", "red")) +  # Blue = negative, Red = positive
  scale_y_continuous(labels = scales::scientific_format()) +  # Scientific notation for y-axis
  labs(title = "Discharge Anomalies (m³) with Trend",
       x = "Year",
       y = "Anomaly (m³)") +
  theme_minimal() +
  theme(legend.position = "none")  # Hide legend since colors are clear

