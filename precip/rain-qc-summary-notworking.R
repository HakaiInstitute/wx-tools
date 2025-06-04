# Load required packages
library(data.table)
library(dplyr)

# Read headers
fileheaders <- read.csv("rain.csv", nrows = 1, header = FALSE, stringsAsFactors = FALSE)

# Read data, skipping redundant headers
df <- fread("rain.csv", skip = 4, col.names = unlist(fileheaders), na.strings = "")

# Rename first column as timestamp and convert to datetime
setnames(df, 1, "timestamp")
df[, timestamp := as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M", tz = "UTC")]

# Select relevant columns (Rain + flags), remove unwanted ones
df <- df[, .SD, .SDcols = c("timestamp", grep("^(Rain|q_flag)", names(df), value = TRUE, ignore.case = TRUE))]
df <- df[, .SD, .SDcols = !grepl("(level|unesco|water year|month|day|year)", names(df), ignore.case = TRUE)]

# Ensure underscore after "Rain", remove all instances of "Q" or "_Q"
setnames(df, gsub("^Rain(?!_)", "Rain_", names(df), perl = TRUE))  # Ensure "Rain_"
setnames(df, gsub("_?Q", "", names(df)))  # Remove "_Q" & "Q"

# Print updated column names
print(names(df))

library(dplyr)
library(tidyr)

# Define the sites you're interested in
sites <- c("BuxtonEast", "Hecate", "Pruth", "SSN1015", "SSN626", "SSN693", "SSN708", "SSN819", "WSN626", "WSN693_703", "WSN703", "WSN703_708", "WSN819_1015", "WSN844")

# Function to apply QC checks
apply_qc <- function(df_site) {
  # Ensure 'flag' is a character column, not a list
  df_site$flag <- NA_character_
  
  # Apply QC checks
  for (i in 1:(nrow(df_site)-1)) {
    # Check if numeric data followed by NA: Flag as 'MV'
    if (!is.na(df_site[i, 2]) && is.na(df_site[i+1, 2])) {
      df_site$flag[i+1] <- "MV"
    }
    
    # Check if 3 consecutive weeks of 0: Flag as 'PV'
    if (i <= nrow(df_site)-3 && all(df_site[i:(i+2), 2] == 0)) {
      df_site$flag[i+2] <- "PV"
    }
    
    # Check if one record differs from another by 10mm: Flag as 'SE'
    if (abs(df_site[i, 2] - df_site[i+1, 2]) > 10) {
      df_site$flag[i+1] <- "SE"
    }
    
    # Check if one value is greater than 8: Flag as 'AR'
    if (df_site[i, 2] > 8) {
      df_site$flag[i] <- "AR"
    }
  }
  
  # If no flags were assigned, flag as 'AV'
  if (all(is.na(df_site$flag))) {
    df_site$flag <- "AV"
  }
  
  return(df_site)
}

# Loop through each site, find the relevant columns, and apply QC
df_sites_split <- lapply(sites, function(site) {
  # Define the column names for the data and flag columns based on the site
  data_column <- paste0("Rain_", site)
  flag_column <- paste0("Rain_", site, "_flags")
  
  # Check if the columns exist in the dataframe
  if (data_column %in% names(df) && flag_column %in% names(df)) {
    # Select relevant columns for the current site
    site_df <- df[, c("timestamp", flag_column, data_column)]
    
    # Check if site_df has rows before applying QC
    if (nrow(site_df) > 0) {
      print(paste("Processing site:", site))  # Diagnostic print
      site_df <- apply_qc(site_df)
      return(site_df)
    } else {
      print(paste("No data for site:", site))  # Diagnostic print
      return(NULL)
    }
  } else {
    print(paste("Columns not found for site:", site))  # Diagnostic print
    return(NULL)
  }
})

# Remove NULL results from sites that don't have columns
df_sites_split <- df_sites_split[!sapply(df_sites_split, is.null)]

# Combine the results from all sites
final_df <- bind_rows(df_sites_split)

# Print final dataframe with QC flags
print(final_df)

