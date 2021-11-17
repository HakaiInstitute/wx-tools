##################################################################################################
#File loading
#################################################################################################
#load packages -- these are ones used commonly for wx QC
lapply(c("tidyverse", "lubridate", "reshape", "stringr", "plotly", "roll", "data.table"), library, character.only = TRUE)

#set working directory
setwd("/YOUR/FILE/LOCATIONS")

# check your working directory is in the right spot
getwd()

# Load data - read headers
fileheaders <- read.csv("2021-11-03.5minuteSamples-raw.csv",
                        nrows = 1, as.is = TRUE,
                        header = FALSE)
# Read in data, drop redundant header rows
data_raw <- read.csv("2021-11-03.5minuteSamples-raw.csv",
                 header = FALSE,
                 skip = 4,
                 stringsAsFactors = FALSE)

# Add headers to dataframe
colnames(data_raw) <- fileheaders
names(data_raw)
glimpse(data_raw)
