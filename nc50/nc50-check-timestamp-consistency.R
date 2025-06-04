# Load necessary libraries
library(dplyr)
library(lubridate)

# Set the directory path where your CSV files are stored
directory_path <- "C:/Users/Emily/Documents/git-repos/wx-tools/NC50-data-qc/working-directory/"

# List all CSV files in the directory
files <- list.files(path = directory_path, pattern = "\\.csv$", full.names = TRUE)

# Define possible timestamp formats (add any additional formats here)
formats <- c("%Y-%m-%d %I:%M:%S %p", "%Y-%m-%d %H:%M:%S", "%d-%m-%Y %H:%M:%S", "%Y-%m-%d %H:%M")

# Function to detect timestamp format
detect_format <- function(timestamp_values, formats) {
  for (format in formats) {
    parsed_times <- parse_date_time(timestamp_values, orders = format, quiet = TRUE)
    if (all(!is.na(parsed_times))) {
      return(format)
    }
  }
  return(NA) # Return NA if no format matched
}

# Initialize an empty dataframe to store results
timestamp_formats <- data.frame(site = character(), detected_format = character(), stringsAsFactors = FALSE)

# Loop through each file
for (file in files) {
  # Read the CSV file
  df <- tryCatch(read.csv(file), error = function(e) NULL)
  if (is.null(df)) next # Skip file if reading fails
  
  # Extract the site name from the file name (assuming the file name is the site name)
  site_name <- tools::file_path_sans_ext(basename(file))
  
  # Ensure the file has at least one column
  if (ncol(df) == 0) {
    timestamp_formats <- timestamp_formats %>%
      add_row(site = site_name, detected_format = NA)
    next
  }
  
  # Take the first column as timestamp (assuming it's the timestamp column)
  timestamp_values <- as.character(df[[1]]) # Convert to character to handle factors or other types
  
  # Detect the format of the timestamp column
  detected_format <- detect_format(timestamp_values, formats)
  
  # Append the results to the dataframe
  timestamp_formats <- timestamp_formats %>%
    add_row(site = site_name, detected_format = detected_format)
}

# View the final dataframe showing the detected formats for each site
print(timestamp_formats)


######
# Collect problematic entries from files
problematic_timestamps <- list()

for (file in files) {
  df <- tryCatch(read.csv(file), error = function(e) NULL)
  if (is.null(df) || ncol(df) == 0) next
  
  timestamp_values <- as.character(df[[1]])
  
  # Identify rows with unparsed timestamps (likely NA after parsing)
  unparsed <- timestamp_values[is.na(parse_date_time(timestamp_values, orders = formats, quiet = TRUE))]
  
  # Save the results for analysis
  if (length(unparsed) > 0) {
    site_name <- tools::file_path_sans_ext(basename(file))
    problematic_timestamps[[site_name]] <- unparsed
  }
}

# Print raw unparsed timestamps for analysis
print(problematic_timestamps)
