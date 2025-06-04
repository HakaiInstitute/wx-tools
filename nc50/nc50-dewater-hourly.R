library(readr)
df_yqq <- read_csv("data/airtemp_cvrd.csv", 
                   col_types = cols(timestamp = col_datetime(format = "%Y-%m-%d %H:%M:%S")))View(df_yqq)     

library(tidyverse)
library(lubridate)

# Set directories
input_directory <- "working-directory"
output_directory <- "data-output"

# List CSV files in the directory
file_list <- list.files(input_directory, pattern = "punt|tsol|tren", full.names = TRUE)

# Function to read, aggregate and save CSV files
process_file <- function(file) {
  # Read the CSV file
  data <- read_csv(file)
  
  # Ensure your timestamp column is in the correct format
  # Replace 'timestamp' with your actual timestamp column name
  data <- data %>%
    mutate(timestamp = ymd_hms(timestamp)) # Adjust based on your timestamp format
  
  # Aggregate to hourly
  hourly_data <- data %>%
    group_by(timestamp = floor_date(timestamp, "hour")) %>%
    summarise(across(everything(), mean, na.rm = TRUE)) # Adjust aggregation method if needed
  
  # Create output file name
  output_file_name <- paste0(tools::file_path_sans_ext(basename(file)), "_hourly.csv")
  output_file_path <- file.path(output_directory, output_file_name)
  
  # Write the aggregated data to a new CSV file
  write_csv(hourly_data, output_file_path)
}

# Process each file
walk(file_list, process_file)

# Set the output directory where hourly files are saved
output_directory <- "data-output"

# List all hourly CSV files in the output directory
hourly_file_list <- list.files(output_directory, pattern = "_hourly\\.csv$", full.names = TRUE)

# Read all hourly files into a list of dataframes
hourly_data_list <- lapply(hourly_file_list, read_csv)

# Optionally, combine all dataframes into one
combined_hourly_data <- bind_rows(hourly_data_list)

# Display the combined data (optional)
print(combined_hourly_data)

merged_df <- inner_join(df_yqq, combined_hourly_data, by = "timestamp")
