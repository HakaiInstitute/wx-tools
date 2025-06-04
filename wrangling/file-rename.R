library(stringr)
library(dplyr)
library(readr)

# File paths
site_file <- "C:/Users/Emily/Documents/git-repos/wx-tools/NC50-data-qc/data/file-naming/site_names.csv"
data_folder <- "C:/Users/Emily/Documents/git-repos/wx-tools/NC50-data-qc/data/file-naming/"

# Read site reference file
site_data <- read_csv(site_file)

# Clean column names
names(site_data) <- tolower(names(site_data))  # serial_number and tidbit_id expected

# List all CSV files
csv_files <- list.files(data_folder, pattern = "\\.hobo$", full.names = TRUE)

# Loop through and rename files
for (file in csv_files) {
  filename <- basename(file)
  
  # Extract serial number (7–8 digit number)
  serial_number <- str_extract(filename, "\\d{7,8}")
  
  if (!is.na(serial_number)) {
    site_row <- site_data %>% filter(as.character(serial_number) == as.character(!!serial_number))
    
    if (nrow(site_row) == 1) {
      site_name <- site_row$tidbit_id  # This is actually your desired site name
      
      # Extract date (format: 2025-05-20)
      date_str <- str_extract(filename, "\\d{4}-\\d{2}-\\d{2}")
      
      if (!is.na(date_str)) {
        date_clean <- gsub("-", "", date_str)  # Convert to 20250520
        new_name <- paste0(site_name, "_", serial_number, "_", date_clean, ".hobo")
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
