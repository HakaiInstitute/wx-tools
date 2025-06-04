# Load necessary libraries
library(dplyr)
library(lubridate)
library(readr) # For writing CSV files

# Set the directory path where your CSV files are stored
directory_path <- "working-directory"

# List all CSV files in the directory
files <- list.files(path = directory_path, pattern = "\\.csv$", full.names = TRUE)

# Initialize an empty dataframe to store results
site_times <- data.frame(
  site = character(),
  start_time = character(),
  stop_time = character(),
  stringsAsFactors = FALSE
)

# Loop through each file
for (file in files) {
  
  # Read the CSV file
  df <- tryCatch(read.csv(file), error = function(e) NULL)
  
  # Skip if the file couldn't be read or is empty
  if (is.null(df) || nrow(df) == 0) {
    message(paste("Skipping empty or unreadable file:", file))
    next
  }
  
  # Extract the site name from the file name (assuming the file name is the site name)
  site_name <- tools::file_path_sans_ext(basename(file))
  
  # Check if the first column exists and has data
  if (ncol(df) < 1 || all(is.na(df[[1]]))) {
    message(paste("No valid data in the timestamp column for file:", file))
    next
  }
  
  # Try parsing the first column (timestamp) with multiple formats using lubridate
  df$timestamp <- parse_date_time(df[[1]], orders = c("%Y-%m-%d %H:%M", "%Y-%m-%d %H:%M:%S"), quiet = TRUE)
  
  # Check if there are any valid timestamps
  if (sum(!is.na(df$timestamp)) == 0) {
    message(paste("No valid timestamps after parsing in file:", file))
    
    # Optional: Print out first few rows of the problematic timestamps for inspection
    print(head(df[[1]], n = 10))
    next
  }
  
  # Get the start and stop times for each site
  start_time <- format(min(df$timestamp, na.rm = TRUE), "%Y-%m-%d %H:%M")
  stop_time <- format(max(df$timestamp, na.rm = TRUE), "%Y-%m-%d %H:%M")
  
  # Append the results to the dataframe
  site_times <- site_times %>%
    add_row(site = site_name, start_time = start_time, stop_time = stop_time)
}

# View the final dataframe
print(site_times)

# Save the QC summary dataframe to a CSV file
output_path <- file.path(directory_path, "timeseries_length.csv")
write_csv(site_times, output_path)

# Notify user of the saved file
message(paste("Timestamp summary saved to:", output_path))
