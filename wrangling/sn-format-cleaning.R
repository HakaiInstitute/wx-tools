
#############################################################################################################################
#load data
#############################################################################################################################
#these are ones used commonly for wx QC
lapply(c("tidyverse", "lubridate", "reshape", "stringr", "plotly", "roll", "data.table"), library, character.only = TRUE)

#set working directory
setwd("/YOUR/FILE/LOCATIONS")

# check your working directory is in the right spot
getwd()

# Load data - read headers
fileheaders <- read.csv("fulmore-2023-qc.csv",
                        nrows = 1, as.is = TRUE,
                        header = FALSE)
# Read in data, drop redundant header rows
df <- read.csv("fulmore-2023-qc.csv",
               header = FALSE,
               skip = 4,
               stringsAsFactors = FALSE)

# Add headers to dataframe
colnames(df) <- fileheaders
names(df)
glimpse(df)

##############################################################################################################################
#restructure
##############################################################################################################################
# select date, level, flags, avg

#list of all possible stations to extract from
station_list<- c("lookout","Buxton", "BuxtonEast","Ethel","Hecate","RefStn","PruthDock","Quadra","WSN819_1015","SSN1015", "SSN1015US","SSN1015DS","WSN626","SSN626",
                "SSN626US","SSN626DS", "SSN693", "SSN693US","SSN693DS","WSN693_703","WSN844","SSN844US","SSN844DS","SSN819","SSN819OS","SSN819US",
                "SSN819DS","WSN703_708","WSN703","SSN703","SSN703US","SSM703DS","Ethel","Koeye")

#################################################################################################################################################################
##prep dataframes
################################################################################################################################################################

library(tidyr)

# Sample data frame
#data <- data.frame(
#  Date = c("2023-10-02 12:00", "2023-10-02 12:05"),
#  rain,buxton,qlevel = c(2, 2),
# rain,buxton,qflag = c("AV", "AV"),
#  rain,buxton = c(0.2, 0.4),
# temp,buxton,qlevel = c(3, 2),
# temp,buxton,qflag = c("EV", "AV"),
# temp,buxton = c(12, 11),
# rain,ssn1015,qlevel = c(2, 2),
#  rain,ssn1015,qflag = c("SVC", "AV"),
#  rain,ssn1015 = c(0.4, 0.2))

#rename date and wrangle column headers
colnames(df)[1] <- "date"
#remove unecessary columns
df <- df %>%
  select(date, -contains("level") | -contains("unesco"))
colnames(df) <- str_replace_all(colnames(df), "_Q_flags", "_qflag")
colnames(df) <- str_replace_all(colnames(df), "Q_flags", "_qflag")
colnames(df) <- str_replace_all(colnames(df), "depth", "depth_")
colnames(df) <- str_replace_all(colnames(df), "twtr", "twtr_")
colnames(df) <- tolower(colnames(df))
#colnames(df) <- str_replace_all(colnames(df), "rain", "rain_")



#separate into character data frame
char_df<-df %>% 
  select(date,contains("flag"))


# Pivot the data using a regular expression in cols
result_df <- pivot_longer(char_df, cols = -date, 
                          names_to = c("variable","site","flag"), 
                          names_sep = "_")
# Remove the unused columns
result_df <- result_df[, -c(4)]

# Rename the columns
colnames(result_df)[4] <- "qflag"

# Print the resulting data frame
print(df)


#separate into numeric data frame

num_df <- df %>% 
  select("date",contains("avg")) 


  num_df <- num_df[, -c(2:4)]

colnames(num_df) <- gsub("avg", "", colnames(num_df))


# pivot
numresult_df <- pivot_longer(num_df, cols = -date, 
                          names_to = c("variable","site"), 
                          names_sep = "_")

#merge numeric and character data frames
merged_df <- merge(result_df, numresult_df, by = c("date", "site", "variable"))

write.csv(merged_df,"merged_df.csv")
