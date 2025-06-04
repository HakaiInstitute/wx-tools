# Load required libraries
library(dplyr)
library(tidyr)
library(tidyverse)
df <- df %>%
  rename(depth = snow_dpeth)


# Define function to detect and clean spikes
clean_spikes <- function(df, threshold) {
  # Calculate the mean and standard deviation
  mean_depth <- mean(df$depth, na.rm = TRUE)
  sd_depth <- sd(df$depth, na.rm = TRUE)
  
  # Identify spikes (values exceeding threshold times the standard deviation)
  spikes <- df$depth > (mean_depth + threshold * sd_depth) | 
    df$depth < (mean_depth - threshold * sd_depth)
  
  # Remove spikes from the dataset
  cleaned_data <- df[!spikes, ]
  
  return(cleaned_data)
}

# Set threshold for spike detection (adjust as needed)
threshold <- 2

# Remove NA values from the depth column while keeping the corresponding timestamps
cleaned_data <- df %>%
  # Filter out rows with NA depth values
  filter(!is.na(depth)) %>%
  # Clean spikes
  clean_spikes(threshold) %>%
  # Complete the time series with missing timestamps and assign NA to depth
  complete(date = seq(min(date), max(date), by = "60 mins")) %>%
  # Assign NA to missing depth values
  mutate(depth = ifelse(is.na(depth), NA, depth))

# Display summary statistics of cleaned data
summary(cleaned_data)

#rate of change
calculate_rate_of_change <- function(data, window_size) {
  data %>%
    mutate(rate_of_change = (depth - lag(depth, default = first(depth))) / window_size)
}

window_size <- 10  # Assuming a window size of 1 day

# Calculate rate of change
cleaned_data_roc <- calculate_rate_of_change(cleaned_data, window_size)

# Set threshold for realistic rate of change (adjust as needed)
threshold <- 10  # Assuming a threshold of 10 units per day

# Remove values that do not have a realistic rate of change
cleaned_data_r <- cleaned_data_roc %>%
  mutate(depth = ifelse(abs(rate_of_change) > threshold, NA, depth))

# Remove the rate_of_change column (optional)
cleaned_data_r <- select(cleaned_data_roc, -rate_of_change)

# Save cleaned df to a new file
write.csv(cleaned_df, "cleaned_snow_depth_df.csv", row.names = FALSE)  # Replace "cleaned_snow_depth_df.csv" with your desired file name

#filter
cleaned_data <- cleaned_data %>%
  filter(is.na(depth) | depth >= 0.05)

###########################
# Load required libraries
library(ggplot2)

# Convert "date" column to Date object if it's not already
cleaned_data$date <- as.Date(cleaned_data$date)

# Extract month and year from the "date" column in abbreviation format
cleaned_data$month_year <- format(cleaned_data$date, "%b %y")

east_buxton<-ggplot(cleaned_data, aes(x = date, y = depth)) +
  geom_line(size = 1) +  # Make the line thicker
  labs(x = "Date (mm/yy)", y = "Snow Depth (m)") +
  scale_x_date(date_labels = "%b %y", date_breaks = "6 months", limits = c(as.Date("2013-09-09"), as.Date("2023-05-01"))) +  # Set date format and breaks
  theme_minimal() +  # Apply a minimal theme for better readability
  theme(
    panel.grid.major = element_line(color = "black"),  # Set color of major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.background = element_rect(fill = "white"),  # Set background color to white
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    axis.ticks.x = element_line(color = "black", size = 0.5)  # Set color and size of x-axis ticks
  )
east_buxton

ggsave("east_buxton_rw.png", east_buxton, width = 10, height = 6, units = "in", dpi = 300)
#############################################################################################
library(dplyr)
library(ggplot2)
require(viridisLite)

# Assuming your data frame is named "cleaned_data" and contains columns "date" for dates and "depth" for snow depth

# Step 1: Filter data for March
march_data <- cleaned_data %>%
  filter(month(date) == 3)

# Step 2: Extract year from date
march_data <- march_data %>%
  mutate(year = lubridate::year(date),
         day = lubridate::day(date))

# Step 3: Aggregate data to daily maximum values for each year
daily_max <- march_data %>%
  group_by(year, day) %>%
  summarize(max_depth = max(depth, na.rm = TRUE))

# Filter data for the specified years
selected_years <- c(2014, 2015, 2016, 2019, 2022)
filtered_data <- daily_max %>% filter(year %in% selected_years)

eb<-ggplot(filtered_data, aes(x = day, y = max_depth, color = as.factor(year))) +
  geom_line(size=1) +
  labs(x = "Day in March", y = "Snow Depth [m]", color = "Year") +
  scale_x_continuous(breaks = 1:31, labels = 1:31) +  # Set breaks and labels for days in March
  scale_color_viridis_d() +  # Use viridis color palette
  theme_minimal() +  # Apply a minimal theme for better readability
  theme(axis.line = element_line(color = "black"),  # Set color of x and y axis lines to black
        panel.border = element_rect(color = "black", fill = NA))

ggsave("eastbuxton-timeseries.png", plot = eb, width = 10, height = 6, units = "in", dpi = 300)
