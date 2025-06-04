######################################################################################################
#read in and wrangle
######################################################################################################
lapply(c("tidyverse", "lubridate", "reshape2", "stringr", "plotly", "roll", "data.table"), library, character.only = TRUE)
install.packages("remotes")
remotes::install_github("HakaiInstitute/hakai-api-client-r", subdir='hakaiApi')
library(hakaiApi)

# Initialize the client
setwd("~")
client <- hakaiApi::Client$new("https://goose.hakai.org/api")

# Request some data from the API
snURL <- "https://goose.hakai.org/api/sn/views/SSN844US:5minuteSamples?limit=100"
data <- client$get(snURL)


start_date <- "2025-05-01T00:00:00Z"  # ISO 8601 format (adjust timezone as needed)
end_date <- "2025-05-10T23:59:59Z"

snURL <- paste0(
  "https://goose.hakai.org/api/sn/views/SSN844US:5minuteSamples?",
  "limit=500&",
  "start=", start_date, "&",
  "end=", end_date
)

data <- client$get(snURL)

# Get the table name from api url for later use
tableName <- tolower(gsub("(outlet|trib)", "_\\1", snURL, ignore.case = TRUE)) %>%
  gsub(".*/", "", .) %>%
  gsub("samples.*$", "", .) %>%
  gsub(":", "_", .)

cat('Database Table to update:', tableName)

# Transform column names: convert to lowercase
colnames(data) <- tolower( colnames(data) )

# Rename the 'Measurement Time' column to 'timestamp' and convert to datetime
colnames(data)[colnames(data) == "measurementtime"] <- "timestamp"

# Glimpse of the dataframe structure
glimpse(data)

# Select columns containing "Avg" and "TWtr" (case-insensitive) for df_twtr
df_twtr_api <- data %>%
  select(timestamp, which(grepl("watertemp_avg|_ql|_qc$|_uql$", colnames(data), ignore.case = TRUE))) %>%
  rename_with(~ gsub("_avg$", "", .))%>%
  rename_with(~ gsub("^.*:", "", .)) 

# Select columns containing "Avg" and "TAir" (case-insensitive) for df_air
df_air_api <- data %>%
  select(timestamp, which(grepl("air_avg|_ql|_qc$|_uql$", colnames(data), ignore.case = TRUE))) %>%
  rename_with(~ gsub("_avg$", "", .))%>%
  rename_with(~ gsub("^.*:", "", .)) 



