library(readr)
library(stringr)
library(stringi)
library(dplyr)
library(lubridate)


# -----------------------------
#  NC50 File pre-upload checks
# -----------------------------


# -----------------------------
#  File name check
# -----------------------------

# File paths
site_file <- "C:/Users/Emily/Documents/git-repos/wx-tools/NC50-data-qc/data/file-naming/site_names.csv"
data_folder <- "C:/Users/Emily/Documents/git-repos/wx-tools/NC50-data-qc/data"

# Read site reference file
site_data <- read_csv(site_file)

# Clean column names
names(site_data) <- tolower(names(site_data))  # serial_number and tidbit_id expected

# List all CSV files
csv_files <- list.files(data_folder, pattern = "\\.csv$", full.names = TRUE)

# Loop through and rename files
for (file in csv_files) {
  filename <- basename(file)
  
  # Extract serial number (7–8 digit number)
  serial_number <- str_extract(filename, "\\d{7,8}")
  
  if (!is.na(serial_number)) {
    site_row <- site_data %>% filter(as.character(serial_number) == as.character(!!serial_number))
    
    if (nrow(site_row) == 1) {
      site_name <- site_row$tidbit_id  # Desired site name
      
      # Extract date (format: 2025-05-20)
      date_str <- str_extract(filename, "\\d{4}-\\d{2}-\\d{2}")
      
      if (!is.na(date_str)) {
        date_clean <- gsub("-", "", date_str)  # Convert to 20250520
        new_name <- paste0(site_name, "_", serial_number, "_", date_clean, ".csv")
        new_path <- file.path(data_folder, new_name)
        
        # Rename the file
        file.rename(file, new_path)
        cat("Renamed:", filename, "->", new_name, "\n")
      } else {
        cat("Date not found in:", filename, "\n")
      }
    } else {
      cat("No match for serial number:", serial_number, "\n")
    }
  } else {
    cat("Serial number not found in:", filename, "\n")
  }
}

# -----------------------------
#  Timestamp check
# -----------------------------

# Set directory
working_directory <- "C:/Users/Emily/Documents/git-repos/wx-tools/NC50-data-qc/data"  # <-- Change this!

# Define expected timestamp format 
expected_format <- "%Y-%m-%d %H:%M"

# Function to check timestamps against expected format
check_timestamp_format <- function(timestamps, expected_formatt) {
  parsed <- suppressWarnings(as.POSIXct(timestamps, format = expected_format, tz = "UTC"))
  list(
    all_good = all(!is.na(parsed)),
    failed_rows = which(is.na(parsed)) + 2  # add 2 to account for skipped header rows
  )
}

# List all CSVs
csv_files <- list.files(working_directory, pattern = "\\.csv$", full.names = TRUE)

# Summary lists
passed_files <- c()
failed_files <- list()

# Process each file
for (file in csv_files) {
  filename <- basename(file)
  cat("Checking:", filename, "\n")
  
  df <- tryCatch(read_csv(file, skip = 2, col_types = cols(.default = "c")),
                 error = function(e) NULL)
  
  if (is.null(df) || ncol(df) < 2) {
    cat("  ERROR: Unable to read or insufficient columns.\n\n")
    failed_files[[filename]] <- "Read error or too few columns"
    next
  }
  
  timestamps <- df[[2]]
  result <- check_timestamp_format(timestamps, expected_format)
  
  if (result$all_good) {
    cat("Passed: all timestamps valid.\n\n")
    passed_files <- c(passed_files, filename)
  } else {
    cat("Failed at rows:", paste(result$failed_rows, collapse = ", "), "\n\n")
    failed_files[[filename]] <- result$failed_rows
  }
}

# -----------------------------
#  Summarize
# -----------------------------
cat("\n--- Summary ---\n")
cat("Passed files:\n")
print(passed_files)

cat("Failed files:\n")
print(failed_files)
