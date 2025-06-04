# Install googlesheets4 if not already installed
# install.packages("googlesheets4")
#column B,E,F,o


# Install googlesheets4 if not already installed
# install.packages("googlesheets4")

# Load the necessary libraries
library(googlesheets4)
library(dplyr)  # For data manipulation

# Authenticate with the correct scope for reading Google Sheets
gs4_auth(scopes = "https://www.googleapis.com/auth/spreadsheets.readonly")

# Specify the Google Sheet URL or ID
sheet_url <- "https://docs.google.com/spreadsheets/d/1AMpk8M5dcB1GqqxMa-L0ByOJQDOar63tXCIirPi2Rq0/edit?gid=1074575717#gid=1074575717"

#specify correct worksheet
df <- read_sheet(sheet_url, sheet = "Sensor Download Summary")

# Select specific columns by name (replace with your actual column names)
# For example, selecting columns "ColumnA", "ColumnB", "ColumnC"
df_selected <- df[, c(2, 5, 6,15)]

# Filter rows that contain "dewater" in any of the selected columns (case-insensitive search)
filtered_df <- df_selected %>%
  filter_all(any_vars(grepl("water", ., ignore.case = TRUE)))

# Print the filtered records
print(filtered_df)

##############################################################################################
#compare 2 sheets
#############################################################################################

library(googlesheets4)
library(dplyr)

# Authenticate with read-only access scope
gs4_auth(
  scopes = "https://www.googleapis.com/auth/spreadsheets.readonly"
)

# Replace with your Google Sheet IDs or URLs
sheet1_id <- "https://docs.google.com/spreadsheets/d/1kPvVvVObp7q8oCHifTIsn_DOms6JJ8nf2q_GLDlG3gk/edit?gid=0#gid=0"  #masterNC50
sheet2_id <- "https://docs.google.com/spreadsheets/d/1AMpk8M5dcB1GqqxMa-L0ByOJQDOar63tXCIirPi2Rq0/edit?gid=132982493#gid=132982493" #old version 

# Read the sheets (assuming they are in the first sheet, adjust 'sheet' parameter if necessary)
sheet1_data <- read_sheet(sheet1_id, sheet = 1)
sheet2_data <- read_sheet(sheet2_id, sheet = 2)

# Assuming the column name is 'serial_number'
# To find matching rows based on 'serial_number'
matching_rows <- inner_join(sheet1_data, sheet2_data, by = "Serial_number")

# To find rows in Sheet 1 but not in Sheet 2
sheet1_only <- anti_join(sheet1_data, sheet2_data, by = "Serial_number")

# To find rows in Sheet 2 but not in Sheet 1
sheet2_only <- anti_join(sheet2_data, sheet1_data, by = "Serial_number")

# View the results
print("Matching Rows:")
print(matching_rows)

print("Rows in Sheet 1 but not in Sheet 2:")
print(sheet1_only)

print("Rows in Sheet 2 but not in Sheet 1:")
print(sheet2_only)

