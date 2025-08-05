# Load necessary libraries
library(dplyr)
library(ggplot2)
library(lubridate)

# Assuming your dataframe is named 'df' and contains columns 'timestamp', 'pls', 'pls2'

df <- read_csv("C:/Users/Emily/Downloads/sensor-network/1015-pls.csv", 
                    col_types = cols(timestamp = col_datetime(format = "%Y-%m-%d %H:%M"), 
                                               pls2 = col_number(), pls = col_number()))

# Step 1: Convert 'timestamp' to Date-Time format
df$timestamp <- as.POSIXct(df$timestamp, format="%Y-%m-%d %H:%M", tz="UTC")

# Step 2: Filter data to include only the period 2019-2025
df <- df %>%
  filter(timestamp >= as.POSIXct("2019-01-01 00:00:00") & timestamp <= as.POSIXct("2025-12-31 23:59:59"))

# Step 3: Extract the month from timestamp to check for seasonality
df$month <- month(df$timestamp)

# Step 4: Plot the data to visualize any differences
ggplot(df, aes(x=timestamp)) +
  geom_line(aes(y=pls, color='Old Sensor'), size=1) +
  geom_line(aes(y=pls2, color='New Sensor'), size=1) +
  labs(title="Comparison of Old and New Sensor Data", x="Date", y="Sensor Reading") +
  theme_minimal() +
  scale_color_manual(values=c("red", "blue"))

#df <- df %>%
 # mutate(pls2 = ifelse(timestamp >= as.POSIXct("2024-08-31 20:10:00") & 
                        # timestamp <= as.POSIXct("2024-08-31 21:15:00"), 
                       #NA, pls2))

# Step 5: Check for the average difference between the sensors by season
df_season <- df %>%
  group_by(month) %>%
  summarize(avg_pls = mean(pls, na.rm = TRUE), avg_pls2 = mean(pls2, na.rm = TRUE))


# Convert month to a factor with month names
df_season$month <- factor(df_season$month, 
                          levels = 1:12, 
                          labels = c("January", "February", "March", "April", "May", 
                                     "June", "July", "August", "September", 
                                     "October", "November", "December"))

ggplot(df_season, aes(x=month)) +
  geom_line(aes(y=avg_pls, color='Old Sensor', group=1), size=1) +  # Explicit group=1 for old sensor
  geom_line(aes(y=avg_pls2, color='New Sensor', group=2), size=1) +  # Explicit group=2 for new sensor
  labs(title="Monthly Average Sensor Readings", x="Month", y="Average Sensor Reading") +
  theme_minimal() +
  scale_color_manual(values=c("red", "blue")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

# Step 5.1: Plot the data to visualize any differences
ggplot(df, aes(x=pls, y=pls2)) +
  geom_point(aes(color='Sensor Comparison'), size=1) +  # Scatterplot of sensor data
  labs(title="Comparison of Old and New Sensor Data", x="Old Sensor (pls)", y="New Sensor (pls2)") +
  theme_minimal() +
  scale_color_manual(values=c("red")) +  # Customize point color
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Optional: Rotate x-axis labels for readability

# First, extract the year from the timestamp
df$year <- format(as.Date(df$timestamp), "%Y")

# Plot the scatterplot by year
ggplot(df, aes(x=pls, y=pls2)) +
  geom_point(aes(color='Sensor Comparison'), size=1) +  # Scatterplot of sensor data
  labs(title="Comparison of Old and New Sensor Data by Year", 
       x="Old Sensor (pls)", 
       y="New Sensor (pls2)") +
  theme_minimal() +
  scale_color_manual(values=c("red")) +  # Customize point color
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Optional: Rotate x-axis labels for readability
  facet_wrap(~ year)  # Creates a separate plot for each year




# Step 6: Linear regression to detect overall offset between old and new sensor
regression_model <- lm(pls2 ~ pls, data = df)
summary(regression_model)

# Step 7: Apply correction to old sensor data
# Calculate the adjustment factor (slope and intercept from regression)
slope <- coef(regression_model)[2]
intercept <- coef(regression_model)[1]

df$pls_corrected <- df$pls * slope + intercept

# Step 8: Plot the corrected data
ggplot(df, aes(x=timestamp)) +
  geom_line(aes(y=pls_corrected, color='Corrected Old Sensor'), size=1) +
  geom_line(aes(y=pls2, color='New Sensor'), size=1) +
  labs(title="Corrected Old Sensor vs New Sensor Data", x="Date", y="Sensor Reading") +
  theme_minimal() +
  scale_color_manual(values=c("green", "blue"))

# Step 9: Optional - Apply seasonality adjustments if needed (e.g., using monthly offset)
# Calculate seasonal adjustments
seasonal_adjustment <- df_season %>%
  mutate(monthly_offset = avg_pls2 - avg_pls)

# Convert month to numeric only for the join, but keep it as a factor for plotting
df$month_numeric <- as.numeric(as.character(df$month))
seasonal_adjustment$month_numeric <- as.numeric(as.character(seasonal_adjustment$month))
# Convert character month names to numeric (1 = January, 2 = February, etc.)
seasonal_adjustment$month_numeric <- match(seasonal_adjustment$month, month.name)


# Perform the join using the numeric month column
df <- df %>%
  left_join(seasonal_adjustment, by = "month_numeric") %>%
  mutate(pls_corrected_seasonal = pls_corrected + monthly_offset)

# Keep month as factor for better graphing
df$month <- factor(df$month, levels = 1:12, labels = month.name)

# Plot with month as a factor
ggplot(df, aes(x = month_numeric)) +
  geom_line(aes(y = pls_corrected_seasonal, color = 'Sensor'), size = 1) +
  labs(title = "Corrected Sensor Readings by Month", x = "Month", y = "Corrected Sensor Reading") +
  theme_minimal() +
  scale_color_manual(values = c("red")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

