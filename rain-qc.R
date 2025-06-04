# Load necessary libraries
library(dplyr)
library(tidyr)
library(data.table)

# Read headers
fileheaders <- read.csv("WSN844.csv", nrows = 1, header = FALSE, stringsAsFactors = FALSE)

# Read data, skipping redundant headers
df <- fread("WSN844.csv", skip = 4, col.names = unlist(fileheaders), na.strings = "")

# names(df) <- tolower(names(df))  # Convert all column names to lowercase

# Rename first column as timestamp and convert to datetime
setnames(df, 1, "timestamp")
#df[, timestamp := as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%s", tz = "America/Los_Angeles")]

# Select relevant columns (Rain + flags), remove unwanted ones
df <- df[, .SD, .SDcols = c("timestamp", grep("^(Rain|q_flag)", names(df), value = TRUE, ignore.case = TRUE))]
df <- df[, .SD, .SDcols = !grepl("(level|unesco|water year|month|day|year)", names(df), ignore.case = TRUE)]

# Step 1: Ensure underscores after "Rain" and handle exceptions
# Add an underscore after "Rain" if it's not already followed by one
names(df) <- gsub("^Rain(?!_)", "Rain_", names(df), perl = TRUE)

# Remove "_Q" and "Q" in other cases
names(df) <- gsub("_?Q", "", names(df))

# Step 2: Remove all instances of "Q" or "_Q" except in "RainWSN844"
# Replace "RainWSN844Q" with "RainWSN844" to preserve the "Q" in WSN844
#names(df) <- gsub("Rainuadra", "Rain_WSN844", names(df))

# Check the updated column names
print(names(df))


# Convert timestamp to POSIXct format
df[, timestamp := as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%S")]


library(dplyr)
library(lubridate)

# Function to perform QC checks
perform_qc_checks <- function(df) {
  
  # Find the index of the first non-NA value in 'Rain_WSN844'
  first_non_na_index <- which(!is.na(df$Rain_WSN844))[1]
  
  # Subset the dataframe from the first non-NA value onwards
  df <- df[first_non_na_index:nrow(df), ]
  
  # Identify runs of consecutive zero values
  zero_runs <- rle(df$Rain_WSN844 == 0)  # Get run-length encoding
  run_lengths <- zero_runs$lengths  # Lengths of runs
  run_values <- zero_runs$values  # Whether it's a zero-run (TRUE/FALSE)
  
  # Identify the starting indices of runs
  run_starts <- cumsum(c(1, head(run_lengths, -1)))  # First index of each run
  
  # Find indices of runs that are at least 8,064 long and contain only zeroes
  long_zero_runs <- which(run_values & run_lengths >= 8064)
  
  # Expand to flag every value in these long zero runs
  flagged_indices <- unlist(mapply(seq, run_starts[long_zero_runs], run_starts[long_zero_runs] + run_lengths[long_zero_runs] - 1))
  
  # Apply QC checks
  df <- df %>%
    mutate(
      check = case_when(
        # If Rain_WSN844 is NA, flag as "MV: Power failure"
        is.na(Rain_WSN844) & is.na(Rain_WSN844_flags) ~ "MV: Power failure",
        
        # If Rain_WSN844 is NA but has a flag, retain the existing flag
        is.na(Rain_WSN844) & !is.na(Rain_WSN844_flags) ~ Rain_WSN844_flags,
        
        # Persistent Value Check: Flag all values within 8,064+ zero sequences
        row_number() %in% flagged_indices ~ "PV: Potential gauge malfunction/blockage",
        
        # Range Check: Flag if difference between consecutive values is greater than 8 mm
        abs(Rain_WSN844 - lag(Rain_WSN844, default = first(Rain_WSN844))) > 8 ~ "SE: Range exceedance",
        
        # If existing flag is not NA, retain the flag from the "Rain_WSN844_flags" column
        !is.na(Rain_WSN844_flags) ~ Rain_WSN844_flags,
        
        # If none of the above conditions are met, and Rain_WSN844 is not NA, flag as 'AV'
        !is.na(Rain_WSN844) ~ "AV",
        
        # If Rain_WSN844 is NA, don't flag anything
        TRUE ~ NA_character_
      )
    )
  
  return(df)
}

# Apply the QC function to the dataframe
df_cleaned <- perform_qc_checks(df)

library(dplyr)

# Count flags based on the first 2 or 3 letters
flag_counts <- df_cleaned %>%
  mutate(flag_prefix = substr(Rain_WSN844_flags, 1, 3)) %>%  # Extract first 3 letters of the flag
  group_by(flag_prefix) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(desc(count))  # Sort by count (descending)

# View flag counts
print(flag_counts)

df_cleaned <- df_cleaned %>%
  select(-Rain_WSN844_flags) %>%  # Remove the original column 2
  rename(Rain_WSN844_flags = check)  # Rename 'check' to the old column 2 name


df_cleaned <- df_cleaned %>%
  mutate(timestamp = format(timestamp, "%Y-%m-%d %H:%M:%S"))

# Export to CSV
write.csv(df_cleaned, "rain/rain_WSN844.csv", row.names = FALSE)

# df_PV <- df_cleaned %>%
#   filter(str_detect(Rain_WSN844_flags, "PV"))
# 
# print(df_PV)

