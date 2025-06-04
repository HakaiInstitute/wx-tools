##################################################################################################
#File loading
#################################################################################################
#load packages -- these are ones used commonly for wx QC
lapply(c("tidyverse", "lubridate", "reshape2", "stringr", "plotly", "roll", "data.table"), library, character.only = TRUE)

#set working directory
setwd("/YOUR/FILE/LOCATIONS")

# check your working directory is in the right spot
getwd()

# Load data - read headers
fileheaders <- read.csv("G2025_WQ_data.csv",
                        nrows = 1, as.is = TRUE,
                        header = FALSE)
# Read in data, drop redundant header rows
g2025 <- read.csv("G2025_WQ_data.csv",
                 header = FALSE,
                 skip = 4,
                 stringsAsFactors = FALSE)

# Add headers to dataframe
colnames(g2025) <- fileheaders
names(g2025)
glimpse(g2025)

#assign new column names if wanted
#names(df) <- c("index", "Date", "abs_pres", "temp","pres", "watlev")
#names(df)



