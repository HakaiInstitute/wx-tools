library(tidyverse)
library(lubridate)
library(zoo)
library(dplyr)
library(data.table)


df$Date<-as.POSIXct(df$Date,format="%Y-%m-%d %H:%M")
str(df)

#WSN626-------------------------------
Qflags <- "WSN626_Q_flags"
Qlevel <- "WSN626_Q_level"
TAir <- "TAirWSN626_Avg"
RH <- "RHWSN626_Avg"
WindDir<-"WindDirWSN626_Avg"
WindSpd<-"WindSpdWSN626_Avg"
Rain<-"RainWSN626_Avg"

Ta_WSN626 <- df %>% 
  select(Date,TAirWSN626_Q_level,TAirWSN626_Q_flags,TAirWSN626_Avg)
colnames(Ta_WSN626) <- c("Date", "Qlevel", "Qflags", "TAir")

Ta_WSN626$Site<-"WSN626"

RH_WSN626 <- df %>% 
  select(Date,RHWSN626_Q_level,RHWSN626_Q_flags,RHWSN626_Avg)
colnames(RH_WSN626) <- c("Date", "Qlevel", "Qflags", "RH")

RH_WSN626$Site <- "WSN626"

WindDir_WSN626 <- df %>% 
  select(Date,WindDirWSN626_Q_level,WindDirWSN626_Q_flag)
colnames(WindDir_WSN626) <- c("Date", "Qlevel", "Qflags", "WindDir")

WindDir_WSN626$Site <- "WSN626"

WindSpd_WSN626 <- df %>% 
  select(Date,WindSpdWSN626_Q_level,WindSpdWSN626_Q_flags,WindSpdWSN626_Avg)
colnames(WindSpd_WSN626) <- c("Date", "Qlevel", "Qflags", "WindSpd")

WindSpd_WSN626$Site <- "WSN626"

 Rain_WSN626 <- df %>% 
  select(Date, Qlevel_RainWSN626, Qflags_RainWSN626, RainWSN626)
colnames( Rain_WSN626) <- c("Date", "Qlevel", "Qflags", " Rain")

 Rain_WSN626$Site <- "WSN626"

#SSN626-------------------------------
Qflags <- "SSN626_Q_flags"
Qlevel <- "SSN626_Q_level"
TAir <- "TAirSSN626_Avg"
RH<- "RHSSN626_Avg"
Rain<-"RainSSN626_Avg"

Ta_SSN626 <- df %>% 
  select(Date,TAirSSN626_Q_level,TAirSSN626_Q_flags,TAirSSN626_Avg)
colnames(Ta_SSN626) <- c("Date", "Qlevel", "Qflags", "TAir")


Ta_SSN626$Site <- "SSN626"

RH_SSN626 <- df %>% 
  select(Date,RHSSN626_Q_level,RHSSN626_Q_flags,RHSSN626_Avg)
colnames(RH_SSN626) <- c("Date", "Qlevel", "Qflags", "RH")

RH_SSN626$Site <- "SSN626"

Rain_SSN626 <- df %>% 
  select(Date, Qlevel_RainSSN626, Qflags_RainSSN626, RainSSN626)
colnames(Rain_SSN626) <- c("Date", "Qlevel", "Qflags", "Rain")

Rain_SSN626$Site <- "SSN626"

#SSN693-------------------------------
Qflags <- "SSN693_Q_flags"
Qlevel <- "SSN693_Q_level"
TAir <- "TAirSSN693_Avg"
RH<-"RHSSN693_Avg"
WindDir<-"WindDirSSN693_Avg"
WindSpd<-"WindSpdSSN693_Avg"
Rain<-"RainSSN693_Avg"

Ta_SSN693 <- df %>% 
  select(Date,TAirSSN693_Q_level,TAirSSN693_Q_flags,TAirSSN693_Avg)
colnames(Ta_SSN693) <- c("Date", "Qlevel", "Qflags", "TAir")


Ta_SSN693$Site <- "SSN693"

RH_SSN693 <- df %>% 
  select(Date,RHSSN693_Q_level,RHSSN693_Q_flags,RHSSN693_Avg)
colnames(RH_SSN693) <- c("Date", "Qlevel", "Qflags", "RH")

RH_SSN693$Site <- "SSN693"

Rain_SSN693 <- df %>% 
  select(Date, Qlevel_RainSSN693, Qflags_RainSSN693, RainSSN693)
colnames(Rain_SSN693) <- c("Date", "Qlevel", "Qflags", "Rain")

Rain_SSN693$Site <- "SSN693"

WindDir_SSN693 <- df %>% 
  select(Date,Qlevel_WindDirSSN693_Avg,Qflag_WindDirSSN693_Avg,WindDirSSN693_Avg)
colnames(WindDir_SSN693) <- c("Date", "Qlevel", "Qflags", "WindDir")

WindDir_SSN693$Site <- "SSN693"

WindSpd_SSN693 <- df %>% 
  select(Date,WindSpdSSN693_Q_level,WindSpdSSN693_Q_flags,WindSpdSSN693_Avg)
colnames(WindSpd_SSN693) <- c("Date", "Qlevel", "Qflags", "WindSpd")

WindSpd_SSN693$Site <- "SSN693"

#WSN693703-------------------------------
Qflags <- "WSN693703_Q_flags"
Qlevel <- "WSN693703_Q_level"
TAir <- "TAirWSN693703_Avg"
RH<-"RHWSN693703_Avg"
Rain<-"WSN693703"
WindDir<-"WindDirWSN693703_Avg"
WindSpd<-"WindSpdWSN693703_Avg"

Ta_WSN693703 <- df %>% 
  select(Date,TAirWSN703_708_Q_level,TAirWSN703_708_Q_flags,TAirWSN693_703_Avg)
colnames(Ta_WSN693703) <- c("Date", "Qlevel", "Qflags", "TAir")


Ta_WSN693703$Site <- "WSN693703"

RH_WSN693703 <- df %>% 
  select(Date,RHWSN693_703_Q_level,RHWSN693_703_Q_flags,RHWSN693_703_Avg)
colnames(RH_WSN693703) <- c("Date", "Qlevel", "Qflags", "RH")

RH_WSN693703$Site <- "WSN693703"

Rain_WSN693_703 <- df %>% 
  select(Date, Qlevel_RainWSN693703, Qflags_RainWSN693703, RainWSN693_703)
colnames(Rain_WSN693_703) <- c("Date", "Qlevel", "Qflags", "Rain")

Rain_WSN693_703$Site <- "WSN693703"

WindDir_WSN693703 <- df %>% 
  select(Date,Qlevel_WindDirWSN693_703_Avg,Qflag_WindSpdWSN693_703_Avg,WindDirWSN693_703_Avg)
colnames(WindDir_WSN693703) <- c("Date", "Qlevel", "Qflags", "WindDir")

WindDir_WSN693703$Site <- "WSN693703"

WindSpd_WSN693703 <- df %>% 
  select(Date,WindSpdWSN693_703_Q_level,WindSpdWSN693_703_Q_flags,WindSpdWSN693_703_Avg)
colnames(WindSpd_WSN693703) <- c("Date", "Qlevel", "Qflags", "WindSpd")

WindSpd_WSN693703$Site <- "WSN693703"

#WSN703-------------------------------
Qflags <- "WSN703_Q_flags"
Qlevel <- "WSN703_Q_level"
TAir <- "TAirWSN703_Avg"
RH <- "RHWSN703_Avg"
Rain<-"WSN703"

Ta_WSN703 <- df %>% 
  select(Date,TAirWSN703_Q_level,TAirWSN703_Q_flags,TAirWSN703_Avg)
colnames(Ta_WSN703) <- c("Date", "Qlevel", "Qflags", "TAir")


Ta_WSN703$Site <- "WSN703"

RH_WSN703 <- df %>% 
  select(Date,RHWSN703_Q_level,RHWSN703_Q_flags,RHWSN703_Avg)
colnames(RH_WSN703) <- c("Date", "Qlevel", "Qflags", "RH")

RH_WSN703$Site <- "WSN703"


Rain_WSN703 <- df %>% 
  select(Date, Qlevel_RainWSN703, Qflags_RainWSN703, RainWSN703)
colnames(Rain_WSN703) <- c("Date", "Qlevel", "Qflags", "Rain")

Rain_WSN703$Site <- "WSN703"

#WSN703708-------------------------------
Qflags <- "WSN703_708_Q_flags"
Qlevel <- "WSN703_708_Q_level"
TAir <- "TAirWSN703_708_Avg"
RH <- "RHWSN703708_Avg"
Rain<-"RainWSN703_708"
WindDir<-"WindDirWSN703708_Avg"
WindSpd<-"WindSpdWSN703708_Avg"

Ta_WSN703708 <- df %>% 
  select(Date,TAirWSN703_708_Q_level,TAirWSN703_708_Q_flags,TAirWSN703_708_Avg)
colnames(Ta_WSN703708) <- c("Date", "Qlevel", "Qflags", "TAir")


Ta_WSN703708$Site <- "WSN703_708"

RH_WSN703708 <- df %>% 
  select(Date,RHWSN703_708_Q_level,RHWSN703_708_Q_flags,RHWSN703_708_Avg)
colnames(RH_WSN703708) <- c("Date", "Qlevel", "Qflags", "RH")

RH_WSN703708$Site <- "WSN703708"

Rain_WSN703_708 <- df %>% 
  select(Date, Qlevel_RainWSN703708, Qflags_RainWSN703708, RainWSN703_708)
colnames(Rain_WSN703_708) <- c("Date", "Qlevel", "Qflags", "Rain")

Rain_WSN703_708$Site <- "WSN703708"

WindDir_WSN703708 <- df %>% 
  select(Date,Qlevel_WindDirWSN703_708_Avg,Qflag_WindDirWSN703_708_Avg,WindDirWSN703_708_Avg)
colnames(WindDir_WSN703708) <- c("Date", "Qlevel", "Qflags", "WindDir")

WindDir_WSN703708$Site <- "WSN703708"

WindSpd_WSN703708 <- df %>% 
  select(Date,WindSpdWSN703_708_Q_level,WindSpdWSN703_708_Q_flags,WindSpdWSN703_708_Avg)
colnames(WindSpd_WSN703708) <- c("Date", "Qlevel", "Qflags", "WindSpd")

WindSpd_WSN703708$Site <- "WSN703708"

#SSN708-------------------------------
Qflags <- "SSN708_Q_flags"
Qlevel <- "SSN708_Q_level"
TAir <- "TAirSSN708_Avg"
RH <- "RHSSN708_Avg"
Rain<-"RainSSN708"

Ta_SSN708 <- df1 %>% 
  select(Date, TAirSSN708_Q_level,TAirSSN708_Q_flags,TAirSSN708_Avg)
colnames(Ta_SSN708) <- c("Date", "Qlevel", "Qflags", "TAir")


Ta_SSN708$Site <- "SSN708"

RH_SSN708 <- df %>% 
  select(Date,RHSSN708_Q_level,RHSSN708_Q_flags,RHSSN708_Avg)
colnames(RH_SSN708) <- c("Date", "Qlevel", "Qflags", "RH")

RH_SSN708$Site <- "SSN708"

Rain_SSN708 <- df %>% 
  select(Date, Qlevel_RainSSN708, Qflags_RainSSN708, RainSSN708)
colnames(Rain_SSN708) <- c("Date", "Qlevel", "Qflags", "Rain")

Rain_SSN708$Site <- "SSN708"

#SSN819-------------------------------
Qflags <- "SSN819_Q_flags"
Qlevel <- "SSN819_Q_level"
TAir <- "TAirSSN819_Avg"
RH <- "RHSSN819_Avg"
Rain<-"SSN819"

Ta_SSN819 <- df %>% 
  select(Date,TAirSSN819_Q_level,TAirSSN819_Q_flags,TAirSSN819_Avg)
colnames(Ta_SSN819) <- c("Date", "Qlevel", "Qflags", "TAir")


Ta_SSN819$Site <- "SSN819"

RH_SSN819 <- df %>% 
  select(Date,RHSSN819_Q_level,RHSSN819_Q_flags,RHSSN819_Avg)
colnames(RH_SSN819) <- c("Date", "Qlevel", "Qflags", "RH")

RH_SSN819$Site <- "SSN819"

Rain_SSN819 <- df %>% 
  select(Date, Qlevel_RainSSN819, Qflags_RainSSN819, RainSSN819)
colnames(Rain_SSN819) <- c("Date", "Qlevel", "Qflags", "Rain")

Rain_SSN819$Site <- "SSN819"

#WSN8191015-------------------------------
Qflags <- "WSN8191015_Q_flags"
Qlevel <- "WSN8191015_Q_level"
TAir <- "TAirWSN8191015_Avg"
RH <- "RHWSN8191015_Avg"
Rain<-"WSN819_1015"
WindDir<-"WindDirWSN8191015_Avg"
WindSpd<-"WindSpdWSN8191015_Avg"

Ta_WSN8191015 <- df %>% 
  select(Date,TAirWSN819_1015_Q_level,TAirWSN819_1015_Q_flags,TAirWSN819_1015_Avg)
colnames(Ta_WSN8191015) <- c("Date", "Qlevel", "Qflags", "TAir")


Ta_WSN8191015$Site <- "WSN8191015"

RH_WSN8191015 <- df %>% 
  select(Date, RHWSN819_1015_Q_level,RHWSN819_1015_Q_flags,RHWSN819_1015_Avg)
colnames(RH_WSN8191015) <- c("Date", "Qlevel", "Qflags", "RH")

RH_WSN8191015$Site <- "WSN8191015"

Rain_WSN819_1015 <- df %>% 
  select(Date, Qlevel_RainWSN8191015, Qflags_RainWSN8191015, RainWSN819_1015)
colnames(Rain_WSN819_1015) <- c("Date", "Qlevel", "Qflags", "Rain")

Rain_WSN819_1015$Site <- "WSN819_1015"

WindDir_WSN8191015 <- df %>% 
  select(Date,Qlevel_WindDirWSN819_1015_Avg,Qflag_WindDirWSN819_1015_Avg,WindDirWSN819_1015_Avg)
colnames(WindDir_WSN8191015) <- c("Date", "Qlevel", "Qflags", "WindDir")

WindDir_WSN8191015$Site <- "WSN8191015"

WindSpd_WSN8191015 <- df %>% 
  select(Date,WindSpdWSN819_1015_Q_level,WindSpdWSN819_1015_Q_flags,WindSpdWSN819_1015_Avg)
colnames(WindSpd_WSN8191015) <- c("Date", "Qlevel", "Qflags", "WindSpd")

WindSpd_WSN8191015$Site <- "WSN8191015"
#WSN844-------------------------------
Qflags <- "WSN844_Q_flags"
Qlevel <- "WSN844_Q_level"
TAir <- "TAirWSN844_Avg"
RH <- "RHWSN844_Avg"
Rain<-"RainWSN844"

Ta_WSN844 <- df %>% 
  select(Date,TAirWSN844_Q_level,TAirWSN844_Q_flags,TAirWSN844_Avg)
colnames(Ta_WSN844) <- c("Date", "Qlevel", "Qflags", "TAir")


Ta_WSN844$Site <- "WSN844"

#RH_WSN844 <- df %>% 
# select(Date,Qlevel_RHWSN844,Qflags_RHWSN844,RHWSN844_Avg)
#colnames(RH_WSN844) <- c("Date", "Qlevel", "Qflags", "RH")

#RH_WSN844$Site <- "WSN844"

Rain_WSN844 <- df %>% 
  select(Date, Qlevel_RainWSN844, Qflags_RainWSN844, RainWSN844)
colnames(Rain_WSN844) <- c("Date", "Qlevel", "Qflags", "Rain")

Rain_WSN844$Site <- "WSN844"


#SSN1015-------------------------------
Qflags <- "SSN1015_Q_flags"
Qlevel <- "SSN1015_Q_level"
TAir <- "TAirSSN1015_Avg"
RH <- "RHSSN1015_Avg"
Rain <- "RainSSN1015"

Ta_SSN1015 <- df %>% 
  select(Date,TAirSSN1015_Q_level,TAirSSN1015_Q_flags,TAirSSN1015_Avg)
colnames(Ta_SSN1015) <- c("Date", "Qlevel", "Qflags", "TAir")


Ta_SSN1015$Site <- "SSN1015"

RH_SSN1015 <- df %>% 
  select(Date,RHSSN1015_Q_level,RHSSN1015_Q_flags,RHSSN1015_Avg)
colnames(RH_SSN1015) <- c("Date", "Qlevel", "Qflags", "RH")

RH_SSN1015$Site <- "SSN1015"

Rain_SSN1015 <- df %>% 
  select(Date, Qlevel_RainSSN1015, Qflags_RainSSN1015, RainSSN1015)
colnames(Rain_SSN1015) <- c("Date", "Qlevel", "Qflags", "Rain")

Rain_SSN1015$Site <- "SSN1015"


#SSN819-------------------------------
Qflags <- "SSN819_Q_flags"
Qlevel <- "SSN819_Q_level"
TAir <- "TAirSSN819_Avg"
RH <- "RHSSN819_Avg"

Ta_SSN819 <- df %>% 
  select(Date,TAirSSN819_Q_level,TAirSSN819_Q_flags,TAirSSN819_Avg)
colnames(Ta_SSN819) <- c("Date", "Qlevel", "Qflags", "TAir")


Ta_SSN819$Site <- "SSN819"

RH_SSN819 <- df %>% 
  select(Date,RHSSN819_Q_level,RHSSN819_Q_flags,RHSSN819_Avg)
colnames(RH_SSN819) <- c("Date", "Qlevel", "Qflags", "RH")

RH_SSN819$Site <- "SSN819"

#Hecate-------------------------------
Qflags <- "Hecate_Q_flags"
Qlevel <- "Hecate_Q_level"
TAir <- "Hecate_Avg"
RH <- "Hecate_Avg"
Rain<-"RainHecate"
WindDir<-"WindDirHecate_Avg"
WindSpd<-"WindSpdHecate_Avg"

Ta_Hecate <- df %>% 
  select(Date,TAirHecateQ_level,TAirHecateQ_flags,TAirHecateAvg)
colnames(Ta_Hecate) <- c("Date", "Qlevel", "Qflags", "TAir")


Ta_Hecate$Site <- "Hecate"

RH_Hecate <- df %>% 
  select(Date,RHHecateQ_level,RHHecateQ_flags,RHHecateAvg)
colnames(RH_Hecate) <- c("Date", "Qlevel", "Qflags", "RH")

RH_Hecate$Site <- "Hecate"

Rain_Hecate <- df %>% 
  select(Date, Qlevel_RainHecate, Qflags_RainHecate, RainHecate)
colnames(Rain_Hecate) <- c("Date", "Qlevel", "Qflags", "Rain")

Rain_Hecate$Site <- "Hecate"

WindDir_Hecate <- df %>% 
  select(Date,Qlevel_WindDirHecateAvg,Qlevel_WindDirHecateAvg,WindDirHecateAvg)
colnames(WindDir_Hecate) <- c("Date", "Qlevel", "Qflags", "WindDir")

WindDir_Hecate$Site <- "Hecate"

WindSpd_Hecate <- df %>% 
  select(Date,WindSpdHecateQ_level,WindSpdHecateQ_flags,WindSpdHecateAvg)
colnames(WindSpd_Hecate) <- c("Date", "Qlevel", "Qflags", "WindSpd")

WindSpd_Hecate$Site <- "Hecate"

#RefStn-------------------------------
Qflags <- "RefStn_Q_flags"
Qlevel <- "RefStn_Q_level"
TAir <- "RefStn_Avg"
RH <- "RefStn_Avg"
Rain<-"RefStn"
TotalP<-"RefStn"
WindDir<-"WindDirRefStn_Avg"
WindSpd<-"WindSpdRefStn_Avg"

Ta_RefStn <- df1 %>% 
  select(Date,TAirRefStnQ_level,TAirRefStnQ_flags,TAirRefStnAvg)
colnames(Ta_RefStn) <- c("Date", "Qlevel", "Qflags", "TAir")


Ta_RefStn$Site <- "RefStn"

RH_RefStn <- df %>% 
  select(Date, RHRefStnQ_level,RHRefStnQ_flags,RHRefStnAvg)
colnames(RH_RefStn) <- c("Date", "Qlevel", "Qflags", "RH")

RH_RefStn$Site <- "RefStn"

Rain_RefStn <- df %>% 
  select(Date, Qlevel_RainRefStn, Qflags_RainRefStn, RainRefStn)
colnames(Rain_RefStn) <- c("Date", "Qlevel", "Qflags", "Rain")

Rain_RefStn$Site <- "RefStn"

TotalP_RefStn <- df %>% 
  select(Date, Qlevel_TotalP_RefStn, Qflags_TotalP_RefStn, TotalP_RefStn)
colnames(TotalP_RefStn) <- c("Date", "Qlevel", "Qflags", "TotalP")

TotalP_RefStn$Site <- "RefStn"

WindDir_RefStn <- df %>% 
  select(Date,Qlevel_WindDirRefStnAvg,Qflag_WindDirRefStnAvg ,WindDirRefStnAvg)
colnames(WindDir_RefStn) <- c("Date", "Qlevel", "Qflags", "WindDir")

WindDir_RefStn$Site <- "RefStn"

WindSpd_RefStn <- df %>% 
  select(Date,WindSpdRefStnQ_level,WindSpdRefStnQ_flags,WindSpdRefStnAvg)
colnames(WindSpd_RefStn) <- c("Date", "Qlevel", "Qflags", "WindSpd")

WindSpd_RefStn$Site <- "RefStn"

#East Buxton-------------------------------
Qflags <- "BuxtonEast_Q_flags"
Qlevel <- "BuxtonEast_Q_level"
TAir <- "BuxtonEast_Avg"
RH <- "BuxtonEast_Avg"
Rain<-"BuxtonEast"
TotalP<-"BuxtonEast"
WindDir<-"WindDirBuxtonEast_Avg"
WindSpd<-"WindSpdBuxtonEast_Avg"

Ta_BuxtonEast <- df %>% 
  select(Date,TAirBuxtonEastQ_level,TAirBuxtonEastQ_flags,TAirBuxtonEastAvg)
colnames(Ta_BuxtonEast) <- c("Date", "Qlevel", "Qflags", "TAir")


Ta_BuxtonEast$Site <- "BuxtonEast"

RH_BuxtonEast <- df %>% 
  select(Date,RHBuxtonEastQ_level, RHBuxtonEastQ_flags,RHBuxtonEastAvg)
colnames(RH_BuxtonEast) <- c("Date", "Qlevel", "Qflags", "RH")

RH_BuxtonEast$Site <- "BuxtonEast"

Rain_BuxtonEast <- df %>% 
  select(Date, Qlevel_RainBuxtonEast, Qflag_RainBuxtonEast, RainBuxtonEast)
colnames(Rain_BuxtonEast) <- c("Date", "Qlevel", "Qflags", "Rain")

Rain_BuxtonEast$Site <- "BuxtonEast"

TotalP_BuxtonEast <- df %>% 
  select(Date, Qlevel_TotalP_BuxtonEast, Qflags_TotalP_BuxtonEast, TotalP_BuxtonEast)
colnames(TotalP_BuxtonEast) <- c("Date", "Qlevel", "Qflags", "TotalP")

TotalP_BuxtonEast$Site <- "BuxtonEast"

WindDir_BuxtonEast <- df %>% 
  select(Date,Qlevel_WindDirBuxtonEastAvg,Qflag_WindDirBuxtonEastAvg,WindDirBuxtonEastAvg)
colnames(WindDir_BuxtonEast) <- c("Date", "Qlevel", "Qflags", "WindDir")

WindDir_BuxtonEast$Site <- "BuxtonEast"

WindSpd_BuxtonEast <- df %>% 
  select(Date,WindSpdBuxtonEastQ_level,WindSpdBuxtonEastQ_flags,WindSpdBuxtonEastAvg)
colnames(WindSpd_BuxtonEast) <- c("Date", "Qlevel", "Qflags", "WindSpd")

WindSpd_BuxtonEast$Site <- "BuxtonEast"

# merge all hourly files to one file
Ta_hourly <- bind_rows(Ta_SSN626, Ta_WSN626)
Ta_hourly <- bind_rows(Ta_hourly, Ta_SSN693)
Ta_hourly <- bind_rows(Ta_hourly, Ta_WSN693703)
Ta_hourly <- bind_rows(Ta_hourly, Ta_WSN703)
Ta_hourly <- bind_rows(Ta_hourly, Ta_SSN708)
Ta_hourly <- bind_rows(Ta_hourly, Ta_WSN703708)
Ta_hourly <- bind_rows(Ta_hourly, Ta_WSN8191015)
Ta_hourly <- bind_rows(Ta_hourly, Ta_SSN819)
Ta_hourly <- bind_rows(Ta_hourly, Ta_SSN1015)
Ta_hourly <- bind_rows(Ta_hourly, Ta_WSN844)
Ta_hourly <- bind_rows(Ta_hourly, Ta_Hecate)
Ta_hourly <- bind_rows(Ta_hourly, Ta_BuxtonEast)
Ta_hourly <- bind_rows(Ta_hourly, Ta_RefStn)

RH_hourly <- bind_rows(RH_SSN626, RH_WSN626)
RH_hourly <- bind_rows(RH_hourly, RH_SSN693)
RH_hourly <- bind_rows(RH_hourly, RH_WSN693703)
RH_hourly <- bind_rows(RH_hourly, RH_WSN703)
RH_hourly <- bind_rows(RH_hourly, RH_SSN708)
RH_hourly <- bind_rows(RH_hourly, RH_WSN703708)
RH_hourly <- bind_rows(RH_hourly, RH_WSN8191015)
RH_hourly <- bind_rows(RH_hourly, RH_SSN819)
RH_hourly <- bind_rows(RH_hourly, RH_SSN1015)
RH_hourly <- bind_rows(RH_hourly, RH_Hecate)
RH_hourly <- bind_rows(RH_hourly, RH_BuxtonEast)
RH_hourly <- bind_rows(RH_hourly, RH_RefStn)


Rain_5min <- bind_rows(Rain_SSN626,Rain_WSN626)
Rain_5min <- bind_rows(Rain_5min, Rain_SSN693)
Rain_5min <- bind_rows(Rain_5min, Rain_WSN693_703)
Rain_5min <- bind_rows(Rain_5min, Rain_WSN703)
Rain_5min <- bind_rows(Rain_5min, Rain_SSN708)
Rain_5min <- bind_rows(Rain_5min, Rain_WSN703_708)
Rain_5min <- bind_rows(Rain_5min, Rain_WSN819_1015)
Rain_5min <- bind_rows(Rain_5min, Rain_SSN819)
Rain_5min <- bind_rows(Rain_5min, Rain_SSN1015)
Rain_5min <- bind_rows(Rain_5min, Rain_Hecate)
Rain_5min <- bind_rows(Rain_5min, Rain_BuxtonEast)
Rain_5min <- bind_rows(Rain_5min, Rain_RefStn)
Rain_5min <- bind_rows(Rain_5min, Rain_WSN844)

Rain_Hourly <- bind_rows(Rain_SSN626,Rain_WSN626)
Rain_Hourly <- bind_rows(Rain_Hourly, Rain_SSN693)
Rain_Hourly <- bind_rows(Rain_Hourly, Rain_WSN693_703)
Rain_Hourly <- bind_rows(Rain_Hourly, Rain_WSN703)
Rain_Hourly <- bind_rows(Rain_Hourly, Rain_SSN708)
Rain_Hourly <- bind_rows(Rain_Hourly, Rain_WSN703_708)
Rain_Hourly <- bind_rows(Rain_Hourly, Rain_WSN819_1015)
Rain_Hourly <- bind_rows(Rain_Hourly, Rain_SSN819)
Rain_Hourly <- bind_rows(Rain_Hourly, Rain_SSN1015)
Rain_Hourly <- bind_rows(Rain_Hourly, Rain_Hecate)
Rain_Hourly <- bind_rows(Rain_Hourly, Rain_BuxtonEast)
Rain_Hourly <- bind_rows(Rain_Hourly, Rain_RefStn)
Rain_Hourly <- bind_rows(Rain_Hourly, Rain_WSN844)
Rain_Hourly <- bind_rows(Rain_Hourly, TotalP_BuxtonEast)
Rain_Hourly <- bind_rows(Rain_Hourly, TotalP_RefStn)


#######5min
# merge all hourly files to one file
Ta_5min <- bind_rows(Ta_SSN626, Ta_WSN626)
Ta_5min <- bind_rows(Ta_5min, Ta_SSN693)
Ta_5min <- bind_rows(Ta_5min, Ta_WSN693703)
Ta_5min <- bind_rows(Ta_5min, Ta_WSN703)
Ta_5min <- bind_rows(Ta_5min, Ta_SSN708)
Ta_5min <- bind_rows(Ta_5min, Ta_WSN703708)
Ta_5min <- bind_rows(Ta_5min, Ta_WSN8191015)
Ta_5min <- bind_rows(Ta_5min, Ta_SSN819)
Ta_5min <- bind_rows(Ta_5min, Ta_SSN1015)
Ta_5min <- bind_rows(Ta_5min, Ta_WSN844)
Ta_5min <- bind_rows(Ta_5min, Ta_Hecate)
Ta_5min <- bind_rows(Ta_5min, Ta_BuxtonEast)
Ta_5min <- bind_rows(Ta_5min, Ta_RefStn)

RH_5min <- bind_rows(RH_SSN626, RH_WSN626)
RH_5min <- bind_rows(RH_5min, RH_SSN693)
RH_5min <- bind_rows(RH_5min, RH_WSN693703)
RH_5min <- bind_rows(RH_5min, RH_WSN703)
RH_5min <- bind_rows(RH_5min, RH_SSN708)
RH_5min <- bind_rows(RH_5min, RH_WSN703708)
RH_5min <- bind_rows(RH_5min, RH_WSN8191015)
RH_5min <- bind_rows(RH_5min, RH_SSN819)
RH_5min <- bind_rows(RH_5min, RH_SSN1015)
RH_5min <- bind_rows(RH_5min, RH_Hecate)
RH_5min <- bind_rows(RH_5min, RH_BuxtonEast)
RH_5min <- bind_rows(RH_5min, RH_RefStn)

WindSpd_5min <- bind_rows(WindSpd_WSN626,WindSpd_SSN693)
WindSpd_5min <- bind_rows(WindSpd_5min, WindSpd_WSN693703)
WindSpd_5min <- bind_rows(WindSpd_5min, WindSpd_WSN703708)
WindSpd_5min <- bind_rows(WindSpd_5min, WindSpd_WSN8191015)
WindSpd_5min <- bind_rows(WindSpd_5min, WindSpd_BuxtonEast)
WindSpd_5min <- bind_rows(WindSpd_5min, WindSpd_Hecate)
WindSpd_5min <- bind_rows(WindSpd_5min, WindSpd_RefStn)

write.csv(WindSpd_5min, "2013-2019_WindSpd_5min_new.csv")

WindDir_Hourly <- bind_rows(WindDir_WSN626,WindDir_SSN693)
WindDir_Hourly <- bind_rows(WindDir_Hourly, WindDir_WSN693703)
WindDir_Hourly <- bind_rows(WindDir_Hourly, WindDir_WSN703708)
WindDir_Hourly <- bind_rows(WindDir_Hourly, WindDir_WSN8191015)
WindDir_Hourly <- bind_rows(WindDir_Hourly, WindDir_BuxtonEast)
WindDir_Hourly <- bind_rows(WindDir_Hourly, WindDir_Hecate)
WindDir_Hourly <- bind_rows(WindDir_Hourly, WindDir_RefStn)


write.csv(WindDir_Hourly, "2013-2019_WindDir_Hourly.csv")
