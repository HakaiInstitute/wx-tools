######################################################################################################
#read in and wrangle
######################################################################################################
lapply(c("tidyverse", "lubridate", "reshape2", "stringr", "plotly", "roll", "data.table","here", "httr2", "remotes"), library, character.only = TRUE)

remotes::install_github("HakaiInstitute/hakai-api-client-r", subdir='hakaiApi')

library(hakaiApi)

# Initialize the client
client <- hakaiApi::Client$new("https://goose.hakai.org/api")

#W50_SalmTrib8_T1,W50_SalmTrib7_T1,W50_SalmTrib6_T1,W50_SalmTrib5_T1,W50_SalmTrib4_T1,W50_SalmTrib3_T1,W50_SalmTrib2_T1,W50_SalmTrib20b_T1,W50_SalmTrib1_T1,W50_SalmTrib19_T1,W50_SalmTrib17_T1,W50_SalmTrib14_T2,W50_SalmTrib11_T1,W50_SalmTrib10_T1,W50_SalmOutlet1_T2,W50_SalmOutlet1_T1:5minuteSamples
#

# List of site names
site_names <- c(
  "W50_CampTrib1_T1",
  "W50_CampTrib5_T1",
  "W50_CampTrib7_T1",
  "W50_CampTrib8_T1",
  "W50_SalmTrib12_T1",
  "W50_SalmTrib13_T1",
  "W50_SalmTrib14_T1",
  "W50_SalmTrib16_T1",
  "W50_StafOutlet2_T1",
  "W50_StafOutlet3_T1",
  "W50_StafOutlet3_T2",
  "W50_WickOutlet2_T1",
  "W50_WickOutlet2_T2",
  "W50_AdamOutlet1_T1",
  "W50_AdamOutlet1_T2",
  "W50_AdamTrib13_T1",
  "W50_AdamTrib14_T1",
  "W50_AdamTrib15_T1",
  "W50_AdamTrib16_T1",
  "W50_AdamTrib17_T1",
  "W50_AdamTrib1_T1",
  "W50_AdamTrib2_T1",
  "W50_AdamTrib3_T1",
  "W50_AdamTrib4_T1",
  "W50_AdamTrib5_T1",
  "W50_AdamTrib6_T1",
  "W50_CampTrib4_T1",
  "W50_CampTrib2_T1",
  "W50_CampTrib3_T1",
  "W50_HyacTrib1_T1",
  "W50_HyacTrib2_T1",
  "W50_SalmTrib17_T1",
  "W50_SalmTrib18_T1",
  "W50_SalmTrib19_T1",
  "W50_SalmTrib1_T1",
  "W50_SalmTrib20b_T1",
  "W50_SalmTrib2_T1",
  "W50_SalmTrib3_T1",
  "W50_SalmTrib4_T1",
  "W50_SalmTrib5_T1",
  "W50_SalmTrib8_T1",
  "W50_ShamOutlet1_T1",
  "W50_TsitOutlet1_T1",
  "W50_TsitTrib1_T1",
  "W50_TsitTrib2_T1",
  "W50_TsitTrib3_T1",
  "W50_TsitTrib5_T1",
  "W50_TsitTrib6_T1"
)

sites <- c("SSN1015US",
           "SSN626US",
           "SSN703US",
           "SSN844US")

components <- c("5minuteSamples.PLS_Lvl", 
                "5minuteSamples.PLS_Temp")

# Base URL format
base_url <- "https://goose.hakai.org/api/sn/views/"

# Generate URLs
urls <- paste0(base_url, variable_names) #":5minuteSamples?limit=-1")


# Function to fetch data for each URL and process it
fetch_and_process_data <- function(url) {
  # Get data for each URL
  response <- client$get(url)
  
  # Extract table name from URL
  table_name <- tolower(gsub("(outlet|trib)", "_\\1", url, ignore.case = TRUE)) %>%
    gsub(".*/", "", .) %>%
    gsub("samples.*$", "", .) %>%
    gsub(":", "_", .)
  
  cat('Database Table to update:', table_name, "\n")
  
  # Convert column names to lowercase
  colnames(data) <- tolower(colnames(data))
  
  # Rename 'measurementtime' to 'timestamp'
  colnames(data)[colnames(data) == "measurementtime"] <- "timestamp"
  

  # Create `df_twtr` with "watertemp_avg" and quality control columns
  df_twtr <- data %>%
    select(timestamp, contains("watertemp_avg", ignore.case = TRUE), ends_with(c("_ql", "_qc", "_uql"))) %>%
    rename_with(~ gsub("_avg$", "", .), contains("_avg")) %>%
    rename_with(~ gsub("^.*:", "", .))
  
  # # Ensure 'timestamp' is formatted as ISO 8601 when saving
  # df_twtr <- df_twtr %>%
  #   mutate(timestamp = ymd_hms(timestamp, tz = "Etc/GMT+7"))
  
  # Return list of processed data frames
  list(table_name = table_name, df_twtr = df_twtr)
}

# Fetch and process data for all URLs  
processed_data <- lapply(urls, fetch_and_process_data)

# Assign processed data frames to the global environment with unique names
invisible(lapply(processed_data, function(dataset) {
  assign(paste0(dataset$table_name, "_twtr"), dataset$df_twtr, envir = .GlobalEnv)
}))

# Define the working directory for saving .csv files
output_directory <- "C:/Users/Emily/Documents/git-repos/wx-tools/NC50-data-qc/working-directory"

# Ensure the directory exists or create it
if (!dir.exists(output_directory)) {
  dir.create(output_directory, recursive = TRUE)
}

# Save processed data frames as .csv files
lapply(processed_data, function(dataset) {
  # Save water temperature dataframe
  if (!is.null(dataset$df_twtr)) {
    write.csv(
      dataset$df_twtr,
      file = file.path(output_directory, paste0(dataset$table_name, "_twtr.csv")),
      row.names = FALSE
    )
  }
})
