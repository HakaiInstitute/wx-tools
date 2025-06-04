# Load necessary libraries
library(readr)
library(stringi)

# Function to check if each CSV file has only valid UTF-8 content
check_utf8_content <- function(directory) {
  # List all CSV files in the directory and subdirectories
  files <- list.files(directory, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
  
  # Initialize a vector to store files with encoding issues
  problem_files <- c()
  
  # Loop through each file and check if all content is valid UTF-8
  for (file in files) {
    # Read file as raw text to check encoding line by line
    lines <- read_lines(file, locale = locale(encoding = "UTF-8"))
    
    # Check if any line has invalid UTF-8 characters
    invalid_lines <- sapply(lines, function(line) {
      !stri_enc_isutf8(line)
    })
    
    # If any line is invalid, add the file to problem_files
    if (any(invalid_lines)) {
      cat("Encoding issue found in file:", file, "\n")
      problem_files <- c(problem_files, file)
    }
  }
  
  # Print summary of files that have invalid UTF-8 content
  if (length(problem_files) > 0) {
    cat("\nFiles with invalid UTF-8 content:\n")
    print(problem_files)
  } else {
    cat("\nAll files have valid UTF-8 content.\n")
  }
}

# Run the function on your directory (replace 'path/to/your/directory' with the actual path)
check_utf8_encoding("C:/Users/Emily/Documents/git-repos/50-watersheds-sensor-data/data")

################################################################################################################################
#file naming consistency
# Load required library
library(stringr)

check_csv_files_for_t1_t2 <- function(directory) {
  cat("Checking directory:", directory, "\n")
  
  # Get all .csv files in the directory and subdirectories
  files <- list.files(directory, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
  cat("Found files:\n", files, "\n")
  
  # Initialize lists to store file names
  failed_files <- list()
  passed_files <- list()
  
  # Iterate over each file
  for (file in files) {
    cat("Processing file:", file, "\n")
    
    # Extract file name without path
    file_name <- basename(file)
    
    # Check if the file name contains "_T1_" or "_T2_"
    if (!str_detect(file_name, "_T1_|_T2_")) {
      failed_files[[file]] <- "File name does not contain '_T1_' or '_T2_'"
    } else {
      passed_files[[file]] <- "File name contains '_T1_' or '_T2_'"
    }
  }
  
  # Print debug information for failed and passed files
  cat("\nFailed files:\n")
  print(failed_files)
  
  cat("\nPassed files:\n")
  print(passed_files)
  
  # Return results as a list
  return(list(passed = names(passed_files), failed = failed_files))
}

# Example usage
directory_path <- "C:/Users/Emily/Documents/git-repos/50-watersheds-sensor-data/data" # Replace with your directory path
check_csv_files_for_t1_t2(directory_path)

# Access and print results
cat("\nSummary:\n")
cat("\nPassed files:\n")
print(results$passed)

cat("\nFailed files and reasons:\n")
print(results$failed)
############################
library(stringr)

check_csv_files_for_naming_convention <- function(directory) {
  cat("Checking directory:", directory, "\n")
  
  # Get all .csv files in the directory and subdirectories
  files <- list.files(directory, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
  cat("Found files:\n", files, "\n")
  
  # Initialize lists to store file names
  failed_files <- list()
  passed_files <- list()
  
  # Regular expression for the file name pattern
  pattern <- "(?i)^(\\d+_)?[a-z]+_[a-z]+\\d{1,2}_(t1|t2)_\\d+_\\d{8}\\.csv$"
  
  # Iterate over each file
  for (file in files) {
    cat("Processing file:", file, "\n")
    
    # Extract file name without path
    file_name <- basename(file)
    
    # Skip files containing "air" (case-insensitive)
    if (str_detect(file_name, "(?i)air")) {
      failed_files[[file]] <- "File name contains 'air', which is not allowed"
      next
    }
    
    # Check if the file name matches the specific pattern
    if (!str_detect(file_name, pattern)) {
      failed_files[[file]] <- "File name does not match required pattern"
    } else {
      passed_files[[file]] <- "File name matches required pattern"
    }
  }
  
  # Print debug information for failed and passed files
  cat("\nFailed files:\n")
  print(failed_files)
  
  cat("\nPassed files:\n")
  print(passed_files)
  
  # Return results as a list
  return(list(passed = names(passed_files), failed = failed_files))
}

# Example usage
directory_path <- "C:/Users/Emily/Documents/git-repos/50-watersheds-sensor-data/data" # Replace with your directory path
results <- check_csv_files_for_naming_convention(directory_path)

# Access and print results
cat("\nSummary:\n")
cat("\nPassed files:\n")
print(results$passed)

cat("\nFailed files and reasons:\n")
print(results$failed)

