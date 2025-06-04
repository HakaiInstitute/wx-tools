library(dplyr)
library(lubridate)
library(readr)  # For reading CSV files
library(purrr)  # For functional programming
library(stringr)  # For string operations
library(tidyr)  # For pivoting the dataframe

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
  
  # Convert 'timestamp' to POSIXct with Pacific Time Zone
  df <- df %>%
    mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "America/Los_Angeles"))  # Adjust timezone as needed
  
  # Ensure the temperature column is the second column
  temperature_col <- names(df)[2]
  
  # Calculate the 24-hour maximum and minimum temperature change
  df <- df %>%
    arrange(timestamp) %>%
    mutate(
      change_24h = !!sym(temperature_col) - lag(!!sym(temperature_col), n = 144)  # 144 5-minute intervals in 12 hours
    )
  
  # Extract the base name of the file (without extension)
  file_name <- tools::file_path_sans_ext(basename(file))
  
  # Create a dynamic qc_flag column name based on the file name
  qc_flag_column <- paste0(file_name, "_qc_flag")
  
  # Apply QC checks and flagging with the dynamic column name
  df_qc <- df %>%
    mutate(
      !!qc_flag_column := case_when(
        !!sym(temperature_col) > 40 ~ "AR: Fail: 40 degC",
        !!sym(temperature_col) < -1 ~ "BR: Fail: -1 degC",
        change_24h > 5.2 ~ paste0("SE: Fail: 24hr change=", round(change_24h, 2)),  # Include the actual 24-hour change value
        TRUE ~ "AV: Pass"
      )
    ) %>%
    select(timestamp, !!sym(temperature_col), !!qc_flag_column)
  
  # Format the timestamp as 'YYYY-MM-DD HH:MM:SS' without any extra characters
  df_qc <- df_qc %>%
    mutate(timestamp = format(timestamp, "%Y-%m-%d %H:%M:%S"))  # Specify desired format
  
  # Generate the output file name by adding "-qc.csv" to the original name
  output_file <- file.path(output_directory, paste0(file_name, "-qc.csv"))
  
  # Save the processed dataframe as a new CSV file
  write_csv(df_qc, output_file)
  
  # Count the number of pass/fail flags per site
  summary <- df_qc %>%
    mutate(
      pass_fail = case_when(
        str_detect(!!sym(qc_flag_column), "Pass") ~ "Pass",
        TRUE ~ "Fail"  # Everything else is considered a fail
      )
    ) %>%
    group_by(pass_fail) %>%
    summarise(count = n(), .groups = 'drop') %>%
    mutate(site = file_name)  # Add site name for reference
  
  return(summary)
}

# Process all CSV files in the input directory
files <- list.files(input_directory, pattern = "*.csv", full.names = TRUE)
qc_summaries <- map_dfr(files, process_csv)

# Reshape the summary to have separate columns for Pass and Fail
qc_summary_df <- qc_summaries %>%
  pivot_wider(names_from = pass_fail, values_from = count, values_fill = 0)

# Only rename if both columns exist
if ("Pass" %in% colnames(qc_summary_df) & "Fail" %in% colnames(qc_summary_df)) {
  qc_summary_df <- qc_summary_df %>%
    rename(Pass = Pass, Fail = Fail)
}

# Save the QC summary dataframe to a CSV file
write_csv(qc_summary_df, file.path(output_directory, "qc_summary_salmon.csv"))
