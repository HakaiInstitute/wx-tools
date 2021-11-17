######################################################################################################
#Conditional statements and gap filling
#################################################################################################
#load packages
lapply(c("tidyverse", "lubridate", "reshape", "stringr", "plotly", "roll", "data.table", "clifro"), library, character.only = TRUE)

#Gap filling NAN values using lag and lead values
df<-df %>% 
  mutate(temp_filled = if_else(is.na(lag(temp)) == F, temp - lag(temp), temp))

#Create or fill detailed Quality_Flag column based on multiple-column data conditions and assign quality level
df$QC_flag <- ifelse(df$Rain > 0 & df$Temp < 1, "SVC: Snowfall: QC'd by EH", ifelse(df$Rain > 0 & df$Temp < 5, "SVC: Potential snowfall: QC'd by EH", "AV: QC'd by EH"))

#Create or fill short form quality flag column and quality level column useful for graphing aesthetics
##Quality levels and flags subjective to scheme group is using
##See Hakai QC standards document
df_qcd<-df %>% 
  mutate(QC_flag_shortened = gsub(":.*","",df$QC_flag),
         QC_Level = ifelse(df$Quality_flag_shortened =="AV", "2", ifelse(df$Quality_flag_shortened =="EV", "3", "2")))

  