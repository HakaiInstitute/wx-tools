# Load necessary libraries
library(tidyverse)
library(lubridate)
library(here)

# Read in data and transform
df <- read.csv(here("data", "2024-11-28.5minuteSamples.all.csv"), header = FALSE, skip = 4, stringsAsFactors = FALSE) %>%
  setNames(unlist(read.csv(here("data", "2024-11-28.5minuteSamples.all.csv"), nrows = 1, header = FALSE, stringsAsFactors = FALSE))) %>%
  rename_with(~ tolower(gsub("(outlet|trib)", "_\\1", gsub("TWtrW50_", "", .))), everything()) %>%
  rename_with(~ gsub("([^_])(outlet|trib)", "\\1_\\2", .), everything()) %>%
  rename(timestamp = !!colnames(.)[1]) %>%
  mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M")) %>%
  select(timestamp, contains("avg", ignore.case = TRUE))

# Read site names and match serial numbers
site_names <- read.csv(here("data", "site_names.csv"))

# Initialize vectors to track unmatched site names and records
unmatched_sites <- c()
unmatched_records <- list()

# Update column names based on partial matching and remove '_avg'
colnames(df)[-1] <- sapply(colnames(df)[-1], function(col_name) {
  matched_site <- site_names %>% filter(str_detect(tolower(col_name), tolower(tidbit_id)))
  if (nrow(matched_site) == 1) {
    new_name <- gsub("_avg", "", paste0(col_name, "_", matched_site$serial_number))
    return(new_name)
  } else {
    unmatched_sites <<- c(unmatched_sites, col_name)
    return(col_name)
  }
})

# Print unmatched site names if any
if (length(unmatched_sites) > 0) {
  cat("Unmatched site names:\n")
  print(unique(unmatched_sites))
} else {
  cat("All site names matched successfully.\n")
}

# Check for records without matching serial numbers
for (col in names(df)[-1]) {
  matched_site <- site_names %>% filter(str_detect(tolower(col), tolower(tidbit_id)))
  
  if (nrow(matched_site) == 1) {
    if (any(is.na(df[[col]]))) {
      unmatched_records[[col]] <- df[is.na(df[[col]]), ]
    }
  }
}

# Print unmatched records
if (length(unmatched_records) > 0) {
  cat("Records without matching serial numbers:\n")
  for (site in names(unmatched_records)) {
    cat(paste("Site:", site, "\n"))
    print(unmatched_records[[site]])
  }
} else {
  cat("All records have matching serial numbers.\n")
}

# Set the output directory and create it if it doesn't exist
working_dir <- here("working-directory")
if (!dir.exists(working_dir)) dir.create(working_dir)

# Loop through each column (excluding 'timestamp') and write to CSV
for (col in names(df)[-1]) {
  first_timestamp <- df$timestamp[1]
  non_na_indices <- which(!is.na(df[[col]]))
  
  # Initialize start index based on the first timestamp
  if (format(first_timestamp, "%H:%M:%S") == "00:00:00") {
    start_index <- if (length(non_na_indices) > 12) non_na_indices[13] else non_na_indices[1]
  } else {
    start_index <- non_na_indices[1]
  }
  
  # Filter the data based on the determined start index
  df_filtered <- df[start_index:nrow(df), ]
  
  # Trim trailing NA values
  last_non_na_index <- max(which(!is.na(df_filtered[[col]])))
  df_filtered <- df_filtered[1:last_non_na_index, ]
  
  # Format the timestamp as a string before writing to CSV
  df_filtered <- df_filtered %>%
    mutate(timestamp = format(timestamp, "%Y-%m-%d %H:%M:%S"))
  
  # Write the filtered dataframe to CSV
  write.csv(df_filtered %>% select(timestamp, all_of(col)), 
            file.path(working_dir, paste0(col, ".csv")), 
            row.names = FALSE)
}

# Confirmation message
cat("CSV files created for each column.\n")
