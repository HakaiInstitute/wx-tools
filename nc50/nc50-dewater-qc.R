library(dplyr)
library(lubridate)
library(readr)  # For reading CSV files
library(purrr)  # For functional programming
library(stringr)  # For string operations

# Set input and output directories
input_directory <- "working-directory"  # Change this to your input directory path
output_directory <- "data-output"  # Output directory for saving processed files

# Create output directory if it doesn't exist
if (!dir.exists(output_directory)) {
  dir.create(output_directory)
}

# Function to process each CSV file
process_csv <- function(file) {
  # Read the CSV file, treating the second column as the temperature column
  df <- read_csv(file, col_types = cols(
    timestamp = col_character(),  # Read timestamp as a character
    .default = col_double()  # Assume all other columns are numeric
  ))
  
  # Convert 'timestamp' to POSIXct
  df <- df %>%
    mutate(timestamp = parse_datetime(timestamp, format = "%Y-%m-%d %H:%M:%S"))  # Parse the timestamp
  
  # Ensure the temperature column is the second column
  temperature_col <- names(df)[2]
  
  # Calculate the 24-hour maximum and minimum temperature change
  df <- df %>%
    arrange(timestamp) %>%
    mutate(
      change_24h = !!sym(temperature_col) - lag(!!sym(temperature_col), n = 144)  # 144 ten-minute intervals in 24 hours
    )
  
  # Apply QC checks and flagging
  df <- df %>%
    mutate(
      qc_flag = case_when(
        !!sym(temperature_col) > 40 ~ "AR",
        !!sym(temperature_col) < -1 ~ "BR",
        change_24h > 5 ~ "SVC",
        TRUE ~ "AV"
      )
    ) %>%
    select(timestamp, !!sym(temperature_col), qc_flag)  # Keep only the required columns
  
  # Extract the base name of the file (without extension)
  file_name <- tools::file_path_sans_ext(basename(file))
  
  # Generate the output file name by adding "_qc.csv" to the original name
  output_file <- file.path(output_directory, paste0(file_name, "-qc.csv"))
  
  # Save the processed dataframe as a new CSV file
  write_csv(df, output_file)
  
  # Return the name of the saved file for confirmation
  return(output_file)
}

# Get a list of all CSV files in the input directory
csv_files <- list.files(input_directory, pattern = "\\.csv$", full.names = TRUE)

# Process and save all CSV files in the input directory
saved_files <- map(csv_files, process_csv)

# Print the names of the saved files for confirmation
print(saved_files)
