# Install required packages if not already installed
# install.packages("googledrive")
# install.packages("readr")
# install.packages("stringr")
# install.packages("stringi")
# install.packages("dplyr")
# install.packages("lubridate")

library(googledrive)
library(readr)
library(stringr)
library(stringi)
library(dplyr)
library(lubridate)

# Load required libraries
library(googledrive)
library(dplyr)
library(stringr)

# Authenticate with Google Drive
drive_auth()

# Replace with your actual Google Drive folder ID
drive_folder_id <- "https://drive.google.com/drive/folders/1VIfg4QhPMaV8Nthm_8j77Qmh1ZsU17a5"

# Define your local folder to save files
data_folder <- "C:/Users/Emily/Documents/git-repos/wx-tools/NC50-data-qc/data"

# List all files in the folder
drive_files <- drive_ls(as_id(drive_folder_id))

# Filter only files with a .hobo extension (case-insensitive)
hobo_files <- drive_files %>%
  filter(str_detect(str_to_lower(name), "\\.hobo$"))

# Download each .hobo file
for (i in seq_len(nrow(hobo_files))) {
  drive_download(
    file = hobo_files[i, ],  # pass entire row to identify file
    path = file.path(data_folder, hobo_files$name[i]),
    overwrite = TRUE
  )
  cat("Downloaded:", hobo_files$name[i], "\n")
}
