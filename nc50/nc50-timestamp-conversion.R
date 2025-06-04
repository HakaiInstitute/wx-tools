# Load necessary library
library(dplyr)

# Function to convert timestamp format: MM-DD-YYYY HH:MM:SS -> YYYY-MM-DD HH:MM
convert_timestamp <- function(date_column) {
  # Convert the string to datetime, then reformat as YYYY-MM-DD HH:MM
  formatted_dates <- format(as.POSIXct(date_column, format = "%m-%d-%Y %H:%M:%S"), "%Y-%m-%d %H:%M")
  return(formatted_dates)
}

# Get list of CSV files in the working directory
csv_files <- list.files(pattern = "*.csv")

# Loop over each CSV file
for (file in csv_files) {
  
  # Read the CSV file, skipping the first two rows (metadata and column names), data starts from row 3
  df <- read.csv(file, header = FALSE, skip = 2, stringsAsFactors = FALSE)
  
  # Check if the file has at least 2 columns to process
  if (ncol(df) < 2) {
    message(paste("Skipping file due to insufficient columns:", file))
    next
  }
  
  # Convert the date-time in the second column (index 2) from MM-DD-YYYY HH:MM:SS to YYYY-MM-DD HH:MM
  df[, 2] <- convert_timestamp(df[, 2])
  
  # Save the modified dataframe back to a new CSV file with the prefix "modified_"
  write.csv(df, paste0("modified_", file), row.names = FALSE)
}

message("Processing complete.")
