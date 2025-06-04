# Load necessary libraries
library(readxl)  # For reading Excel files
library(dplyr)   # For data manipulation
library(lubridate) # For date-time handling
library(writexl) # For writing Excel files (not used here but good to have)
library(data.table)
library(tidyverse)

# Define the input and output directories
input_dir<- "C:/Users/Emily/Documents/git-repos/wx-tools/NC50-data/input_data"
output_dir <- "C:/Users/Emily/Documents/git-repos/wx-tools/NC50-data/working_directory"


# Function to parse different timestamp formats
parse_timestamp <- function(timestamps) {
  formats <- c("%Y-%m-%d %H:%M", "%m-%d-%Y %H:%M", "%d-%m-%Y %H:%M", "%Y/%m/%d %H:%M", 
               "%m/%d/%Y %H:%M", "%d/%m/%Y %H:%M", "%Y-%m-%d %H:%M:%S", "%m-%d-%Y %H:%M:%S", 
               "%d-%m-%Y %H:%M:%S", "%Y/%m/%d %H:%M:%S", "%m/%d/%Y %H:%M:%S", "%d/%m/%Y %H:%M:%S")
  
  parsed_times <- suppressWarnings(parse_date_time(timestamps, orders = formats))
  return(parsed_times)
}

# Get a list of all files in the input directory
file_list <- list.files(input_dir, full.names = TRUE)

# Process each file
for (file in file_list) {
  if (grepl("\\.xlsx$", file, ignore.case = TRUE)) {
    # Read the .xlsx file
    df <- read_excel(file)
  } else if (grepl("\\.csv$", file, ignore.case = TRUE)) {
    # Read the .csv file with fread for faster performance
    df <- fread(file)
  } else {
    next
  }
  
  # Remove the first column
  df <- df[, -1, with = FALSE]
  
  # If there is a header, remove it (assuming first row is the header)
  if (nrow(df) > 0) {
    df <- df[-1, ]
  }
  
    # Rename the new first and second columns
  colnames(df)[1:2] <- c("timestamp", "temperature")
  
  # Remove any extra columns after renaming (should only have "timestamp" and "temperature")
  df <- df %>%
    select(timestamp, temperature)
  
  # Parse and standardize timestamps
  df$timestamp <- parse_timestamp(df$timestamp)
  df$timestamp <- format(df$timestamp, "%Y-%m-%d %H:%M:%S")
  
  # Save as .csv using fwrite for faster performance
  output_file <- file.path(output_dir, paste0(tools::file_path_sans_ext(basename(file)), ".csv"))
  fwrite(df, output_file)
}
