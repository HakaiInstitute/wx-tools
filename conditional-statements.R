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
df_qcd<-df %>% 
  mutate(QC_flag_shortened = gsub(":.*","",df$QC_flag),
         QC_Level = ifelse(df$Quality_flag_shortened =="AV", "2", ifelse(df$Quality_flag_shortened =="EV", "3", "2")))

df2<-df %>% 
  mutate(WSN703_708q = ifelse(RHWSN703_708_Q_flags == "SVC", rowMeans(across(V19, V22, V40), na.rm = TRUE)), RHWSN703_708_Avg)


########################################################################################################################### 
regex cheats

anchors

^ -- matches any string that starts with 
$ -- matches any string that ends with 
^$ -- matches exact string
roar -- matches any string that has roar in it

quantifiers

abc* -- matches a string that has ab followed by zero or more c
abc+ -- matches a string that has ab followed by one or more c
abc? -- matches a string that has ab followed by zero or one c
abc{2} -- matches a string that has ab followed by 2 c
abc{2,} -- matches a string that has ab followed by 2 or more c
abc{2,5} -- matches a string that has ab followed by 2 up to 5 c
a(bc)* -- matches a string that has a followed by zero or more copies of the sequence bc
a(bc){2,5} -- matches a string that has a followed by 2 up to 5 copies of the sequence bc


elements from a vector or array in R = square brackets
[,1] -- all rows but only second column 

(Qflag|Qlevel)?_
(\S+(?=_Avg))