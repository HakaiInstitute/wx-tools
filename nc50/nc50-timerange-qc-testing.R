library(dplyr)
library(lubridate)
library(readr)  # For reading CSV files
library(purrr)  # For functional programming
library(stringr)  # For string operations
library(tidyr)  # For pivoting the dataframe

# Define the list of timestamp ranges
# Each element in the list should be a vector of two POSIXct timestamps specifying the start and end of a range
timestamp_ranges <- list(
  c(as.POSIXct("2024-08-04 00:00:00", tz = "America/Los_Angeles"),
    as.POSIXct("2024-08-21 12:00:00", tz = "America/Los_Angeles")))
  
# Function to check if each timestamp falls within any range
is_in_ranges <- function(timestamps, ranges) {
  sapply(timestamps, function(ts) {
    any(sapply(ranges, function(range) {
      ts >= range[1] & ts <= range[2]
    }))
  })
}


# Function to process a specified CSV file
process_csv <- function(file) {
  # Read the CSV file, treating the second column as the temperature column
  df <- read_csv(file, col_types = cols(
    timestamp = col_character(),  # Read timestamp as a character
    .default = col_double()  # Assume all other columns are numeric
  ))
  
  # Convert 'timestamp' to POSIXct with Pacific Time Zone
  df <- df %>%
    mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "America/Los_Angeles"))
  
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
  
  df_qc <- df %>%
    mutate(
      !!qc_flag_column := case_when(
        is_in_ranges(timestamp, timestamp_ranges) ~ "SVD: Specified Range",
        !!sym(temperature_col) > 40 ~ "AR: Fail: 40 degC",
        !!sym(temperature_col) < -1 ~ "BR: Fail: -1 degC",
        change_24h > 5.2 ~ paste0("SE: Fail: 24hr change=", round(change_24h, 2)),  # Include the actual 24-hour change value
        TRUE ~ "AV: Pass"
      )
    ) %>%
    select(-change_24h)  # Remove the 24-hour change column
  
  
  
  # Format the timestamp as 'YYYY-MM-DD HH:MM:SS' without any extra characters
  df_qc <- df_qc %>%
    mutate(timestamp = format(timestamp, "%Y-%m-%d %H:%M:%S"))
  
  # Generate the output file name by adding "-qc.csv" to the original name
  output_file <- paste0(file_name, "-qc.csv")
  
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

# Specify the file to process
file_to_process <- "C:/Users/Emily/Documents/git-repos/wx-tools/NC50-data-qc/data-output/svc-files/testing/salm_trib4_t1_21724415.csv"  # Replace with the path to your file
qc_summary <- process_csv(file_to_process)

# Print QC summary for the specified file
print(qc_summary)
