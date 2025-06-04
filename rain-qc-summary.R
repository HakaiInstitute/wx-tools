library(dplyr)
library(stringr)
library(readr)

# Set working directory (optional) 
# setwd("path/to/your/folder")  

# List all CSV files in the directory
csv_files <- list.files(pattern = "\\.csv$")

# Initialize empty list to store results
qc_summary_list <- list()

# Loop through each CSV file
for (file in csv_files) {
  
  # Extract site name from filename (removes .csv extension)
  site_name <- tools::file_path_sans_ext(basename(file))
  
  # Read CSV (assuming flags are in the 3rd column)
  df <- read_csv(file, col_types = cols(.default = "c"))  # Read as characters
  
  # Ensure the file has at least 3 columns
  if (ncol(df) < 3) next
  
  # Extract QC flags from the 3rd column
  qc_flags <- df[[3]]
  
  # Adjust the regular expression to handle flags with or without a colon
  # This regular expression ensures that we match either "AV", "MV", "PV", or any flag that may or may not have a colon
  qc_flags <- str_extract(toupper(qc_flags), "^[A-Z]{2,3}(?::)?")
  
  # Remove the colon for consistency
  qc_flags <- str_replace_all(qc_flags, ":", "")
  
  # Count occurrences of each QC flag
  flag_counts <- table(qc_flags)
  
  # Convert to data frame
  flag_df <- as.data.frame(flag_counts) %>%
    rename(flag = qc_flags, count = Freq) %>%
    pivot_wider(names_from = flag, values_from = count, values_fill = list(count = 0)) 
  
  # Ensure columns match expected flags (AV, MV, PV, SVC, SVD, SE, CE)
  expected_flags <- c("AV", "MV", "PV", "SVC", "SVD", "SE", "CE", "SV")
  flag_df <- flag_df %>%
    select(any_of(expected_flags)) %>%
    mutate(across(everything(), ~ replace_na(.x, 0)))  # Fill missing flags with 0
  
  # Add site name
  flag_df <- flag_df %>%
    mutate(site = site_name) %>%
    relocate(site)  # Ensure site column is first
  
  # Store in list
  qc_summary_list[[site_name]] <- flag_df
}

# Combine all site summaries into a single table
qc_summary_table <- bind_rows(qc_summary_list)

# Print summary table
print(qc_summary_table)

# Save summary as CSV
write_csv(qc_summary_table, "QC_Flag_Summary.csv")
