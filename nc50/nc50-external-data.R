library(dplyr)
library(purrr)
library(stringr)

# Set directory path
directory_path <- "data"

# Get all files in the directory that match the patterns
files <- list.files(directory_path, pattern = "MoTIe|EC|FLNRO", full.names = TRUE)

# Function to extract the chunk of string between two underscores
extract_df_name <- function(file) {
  # Extract file name without the directory path and file extension
  file_name <- basename(file)
  # Extract the chunk between the first two underscores
  chunk <- str_extract(file_name, "(?<=_)[^_]+(?=_)")
  # Create a dataframe name with 'df_' prefix
  df_name <- paste0("df_", chunk)
  return(df_name)
}

# Function to read in the CSV, skip the first row, and use the second row as headers
read_custom_csv <- function(file) {
  df <- read.csv(file, skip = 1, header = TRUE)
  
  # Rename 'time' column to 'timestamp' if it exists
  if ("time" %in% names(df)) {
    df <- df %>%
      rename(timestamp = time)
  }
  
  return(df)
}

# Read in files and assign each to a variable named after the chunk between underscores
for (file in files) {
  df_name <- extract_df_name(file)  # Get dataframe name
  assign(df_name, read_custom_csv(file))  # Read CSV and assign to variable
}
