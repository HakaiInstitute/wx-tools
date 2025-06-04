library(tidyverse)
library(lubridate)
library(zoo)
library(dplyr)
library(data.table)


df$Date<-as.POSIXct(df$Date,format="%Y-%m-%d %H:%M")
str(df)

#depth-----------------------------------------------------------------------------------
df<-na.omit(df)
#
Depth_Lull <- df %>% 
  select(Date,DepthLULL1PT_Q_level,DepthLULL1PT_Q_flags,DepthLULL1PT_Avg)
colnames(Depth_Lull) <- c("Date", "Qlevel", "Qflags", "Stage")

Depth_Lull$Site<-"Lull"

#
Depth_Full <- df %>% 
  select(Date,DepthFULL1PT_Q_level,DepthFULL1PT_Q_flags,DepthFULL1PT_Avg)
colnames(Depth_Full) <- c("Date", "Qlevel", "Qflags", "Stage")

Depth_Full$Site<-"Fulmore"

#
Depth_Tuna <- df %>% 
  select(Date,DepthTUNA1PT_Q_level,DepthTUNA1PT_Q_flags,DepthTUNA1PT_Avg)
colnames(Depth_Tuna) <- c("Date", "Qlevel", "Qflags", "Stage")

Depth_Tuna$Site<-"Tuna"

#
Depth_Heydon <- df %>% 
  select(Date,DepthHEYD2PT_Q_level,DepthHEYD2PT_Q_flags,DepthHEYD2PT_Avg)
colnames(Depth_Heydon) <- c("Date", "Qlevel", "Qflags", "Stage")

Depth_Heydon$Site<-"Heydon"

#
Depth_Glendale <- df %>% 
  select(Date,DepthGLEN1PT_Q_level,DepthGLEN1PT_Q_flags,DepthGLEN1PT_Avg)
colnames(Depth_Glendale) <- c("Date", "Qlevel", "Qflags", "Stage")

Depth_Glendale$Site<-"Glendale"

daily <- bind_rows(Depth_Full, Depth_Glendale)
daily <- bind_rows(daily, Depth_Tuna)
daily <- bind_rows(daily, Depth_Heydon)
daily <- bind_rows(daily, Depth_Lull)

#-------------------------------------------------------------------------
#temp
df<-na.omit(df)

temp_Lull <- df %>% 
  select(Date,TWtrLULL1PT_Q_level,TWtrLULL1PT_Q_flags,TWtrLULL1PT_Avg)
colnames(temp_Lull) <- c("Date", "Qlevel", "Qflags", "temp")

temp_Lull$Site<-"Lull"

#
temp_Full <- df %>% 
  select(Date,TWtrFULL1PT_Q_level,TWtrFULL1PT_Q_flags,TWtrFULL1PT_Avg)
colnames(temp_Full) <- c("Date", "Qlevel", "Qflags", "temp")

temp_Full$Site<-"Fulmore"

#
temp_Tuna <- df %>% 
  select(Date,TWtrTUNA1PT_Q_level,TWtrTUNA1PT_Q_flags,TWtrTUNA1PT_Avg)
colnames(temp_Tuna) <- c("Date", "Qlevel", "Qflags", "temp")

temp_Tuna$Site<-"Tuna"

#
temp_Heydon <- df %>% 
  select(Date,TWtrHEYD2PT_Q_level,TWtrHEYD2PT_Q_flags,TWtrHEYD2PT_Avg)
colnames(temp_Heydon) <- c("Date", "Qlevel", "Qflags", "temp")

temp_Heydon$Site<-"Heydon"

#
temp_Glendale <- df %>% 
  select(Date,TWtrGLEN1PT_Q_level,TWtrGLEN1PT_Q_flags,TWtrGLEN1PT_Avg)
colnames(temp_Glendale) <- c("Date", "Qlevel", "Qflags", "temp")

temp_Glendale$Site<-"Glendale"

#
tb1_tuna <- df %>% 
  select(Date,TWtrTUNA1_TB1_Q_level,TWtrTUNA1_TB1_Q_flags,TWtrTUNA1_TB1_Avg)
colnames(tb1_tuna) <- c("Date", "Qlevel", "Qflags", "temp")

tb1_tuna$Site<-"Tuna"

#
tb1_lull <- df %>% 
  select(Date,TWtrLULL1_TB1_Q_level,TWtrLULL1_TB1_Q_flags,TWtrLULL1_TB1_Avg)
colnames(tb1_lull) <- c("Date", "Qlevel", "Qflags", "temp")

tb1_lull$Site<-"Lull"

#
tb1_full <- df %>% 
  select(Date,TWtrFULL1_TB1_Q_level,TWtrFULL1_TB1_Q_flags,TWtrFULL1_TB1_Avg)
colnames(tb1_full) <- c("Date", "Qlevel", "Qflags", "temp")

tb1_full$Site<-"Fulmore"

tempdaily <- bind_rows(temp_Full, temp_Lull)
tempdaily <- bind_rows(tempdaily, temp_Tuna)
tempdaily <- bind_rows(tempdaily, temp_Heydon)
tempdaily <- bind_rows(tempdaily, temp_Glendale)
tempdaily <- bind_rows(tempdaily, tb1_lull)
tempdaily <- bind_rows(tempdaily, tb1_tuna)
tempdaily <- bind_rows(tempdaily, tb1_full)

totdaily <- bind_rows(daily, tempdaily)

#------------------------------------------------------------------
#WSN626-------------------------------
Qflags <- "WSN626_Q_flags"
Qlevel <- "WSN626_Q_level"
TAir <- "TAirWSN626_Avg"
RH <- "RHWSN626_Avg"

RH_WSN626 <- df %>% 
  select(Date,RHWSN626_Q_level,RHWSN626_Q_flags,RHWSN626_Avg)
colnames(RH_WSN626) <- c("Date", "Qlevel", "Qflags", "RH")

RH_WSN626$Site <- "WSN626"

#SSN626-------------------------------
Qflags <- "SSN626_Q_flags"
Qlevel <- "SSN626_Q_level"
TAir <- "TAirSSN626_Avg"
RH<- "RHSSN626_Avg"

Ta_SSN626 <- df %>% 
  select(Date,TAirSSN626_Q_level,TAirSSN626_Q_flags,TAirSSN626_Avg)
colnames(Ta_SSN626) <- c("Date", "Qlevel", "Qflags", "TAir")


Ta_SSN626$Site <- "SSN626"

RH_SSN626 <- df %>% 
  select(Date,RHSSN626_Q_level,RHSSN626_Q_flags,RHSSN626_Avg)
colnames(RH_SSN626) <- c("Date", "Qlevel", "Qflags", "RH")

RH_SSN626$Site <- "SSN626"

#SSN693-------------------------------
Qflags <- "SSN693_Q_flags"
Qlevel <- "SSN693_Q_level"
TAir <- "TAirSSN693_Avg"
RH<-"RHSSN693_Avg"

Ta_SSN693 <- df %>% 
  select(Date,TAirSSN693_Q_level,TAirSSN693_Q_flags,TAirSSN693_Avg)
colnames(Ta_SSN693) <- c("Date", "Qlevel", "Qflags", "TAir")


Ta_SSN693$Site <- "SSN693"

RH_SSN693 <- df %>% 
  select(Date,RHSSN693_Q_level,RHSSN693_Q_flags,RHSSN693_Avg)
colnames(RH_SSN693) <- c("Date", "Qlevel", "Qflags", "RH")

RH_SSN693$Site <- "SSN693"

#WSN693703-------------------------------
Qflags <- "WSN693703_Q_flags"
Qlevel <- "WSN693703_Q_level"
TAir <- "TAirWSN693703_Avg"
RH<-"RHWSN693703_Avg"

Ta_WSN693703 <- df %>% 
  select(Date,TAirWSN693_703_Q_level,TAirWSN693_703_Q_flags,TAirWSN693_703_Avg)
colnames(Ta_WSN693703) <- c("Date", "Qlevel", "Qflags", "TAir")


Ta_WSN693703$Site <- "WSN693703"

RH_WSN693703 <- df %>% 
  select(Date,RHWSN693_703_Q_level,RHWSN693_703_Q_flags,RHWSN693_703_Avg)
colnames(RH_WSN693703) <- c("Date", "Qlevel", "Qflags", "RH")

RH_WSN693703$Site <- "WSN693703"

#WSN703-------------------------------
Qflags <- "WSN703_Q_flags"
Qlevel <- "WSN703_Q_level"
TAir <- "TAirWSN703_Avg"
RH <- "RHWSN703_Avg"

Ta_WSN703 <- df %>% 
  select(Date,TAirWSN703_Q_level,TAirWSN703_Q_flags,TAirWSN703_Avg)
colnames(Ta_WSN703) <- c("Date", "Qlevel", "Qflags", "TAir")


Ta_WSN703$Site <- "WSN703"

RH_WSN703 <- df %>% 
  select(Date,RHWSN703_Q_level,RHWSN703_Q_flags,RHWSN703_Avg)
colnames(RH_WSN703) <- c("Date", "Qlevel", "Qflags", "RH")

RH_WSN703$Site <- "WSN703"

#WSN703708-------------------------------
Qflags <- "WSN703_708_Q_flags"
Qlevel <- "WSN703_708_Q_level"
TAir <- "TAirWSN703_708_Avg"
RH <- "RHWSN703708_Avg"

Ta_WSN703708 <- df %>% 
  select(Date,TAirWSN703_708_Q_level,TAirWSN703_708_Q_flags,TAirWSN703_708_Avg)
colnames(Ta_WSN703708) <- c("Date", "Qlevel", "Qflags", "TAir")


Ta_WSN703708$Site <- "WSN703_708"

RH_WSN703708 <- df %>% 
  select(Date,RHWSN703_708_Q_level,RHWSN703_708_Q_flags,RHWSN703_708_Avg)
colnames(RH_WSN703708) <- c("Date", "Qlevel", "Qflags", "RH")

RH_WSN703708$Site <- "WSN703708"

#SSN708-------------------------------
Qflags <- "SSN708_Q_flags"
Qlevel <- "SSN708_Q_level"
TAir <- "TAirSSN708_Avg"
RH <- "RHSSN708_Avg"

Ta_SSN708 <- df1 %>% 
  select(Date, TAirSSN708_Q_level,TAirSSN708_Q_flags,TAirSSN708_Avg)
colnames(Ta_SSN708) <- c("Date", "Qlevel", "Qflags", "TAir")


Ta_SSN708$Site <- "SSN708"

RH_SSN708 <- df %>% 
  select(Date,RHSSN708_Q_level,RHSSN708_Q_flags,RHSSN708_Avg)
colnames(RH_SSN708) <- c("Date", "Qlevel", "Qflags", "RH")

RH_SSN708$Site <- "SSN708"

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

#WSN8191015-------------------------------
Qflags <- "WSN8191015_Q_flags"
Qlevel <- "WSN8191015_Q_level"
TAir <- "TAirWSN8191015_Avg"
RH <- "RHWSN8191015_Avg"

Ta_WSN8191015 <- df %>% 
  select(Date,TAirWSN819_1015_Q_level,TAirWSN819_1015_Q_flags,TAirWSN819_1015_Avg)
colnames(Ta_WSN8191015) <- c("Date", "Qlevel", "Qflags", "TAir")


Ta_WSN8191015$Site <- "WSN8191015"

RH_WSN8191015 <- df %>% 
  select(Date, RHWSN819_1015_Q_level,RHWSN819_1015_Q_flags,RHWSN819_1015_Avg)
colnames(RH_WSN8191015) <- c("Date", "Qlevel", "Qflags", "RH")

RH_WSN8191015$Site <- "WSN8191015"

#WSN844-------------------------------
Qflags <- "WSN844_Q_flags"
Qlevel <- "WSN844_Q_level"
TAir <- "TAirWSN844_Avg"
RH <- "RHWSN844_Avg"

Ta_WSN844 <- df %>% 
  select(Date,TAirWSN844_Q_level,TAirWSN844_Q_flags,TAirWSN844_Avg)
colnames(Ta_WSN844) <- c("Date", "Qlevel", "Qflags", "TAir")


Ta_WSN844$Site <- "WSN844"

#RH_WSN844 <- df %>% 
 # select(Date,Qlevel_RHWSN844,Qflags_RHWSN844,RHWSN844_Avg)
#colnames(RH_WSN844) <- c("Date", "Qlevel", "Qflags", "RH")

#RH_WSN844$Site <- "WSN844"

#SSN1015-------------------------------
Qflags <- "SSN1015_Q_flags"
Qlevel <- "SSN1015_Q_level"
TAir <- "TAirSSN1015_Avg"
RH <- "RHSSN1015_Avg"

Ta_SSN1015 <- df %>% 
  select(Date,TAirSSN1015_Q_level,TAirSSN1015_Q_flags,TAirSSN1015_Avg)
colnames(Ta_SSN1015) <- c("Date", "Qlevel", "Qflags", "TAir")


Ta_SSN1015$Site <- "SSN1015"

RH_SSN1015 <- df %>% 
  select(Date,RHSSN1015_Q_level,RHSSN1015_Q_flags,RHSSN1015_Avg)
colnames(RH_SSN1015) <- c("Date", "Qlevel", "Qflags", "RH")

RH_SSN1015$Site <- "SSN1015"

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

Ta_Hecate <- df %>% 
  select(Date,TAirHecateQ_level,TAirHecateQ_flags,TAirHecateAvg)
colnames(Ta_Hecate) <- c("Date", "Qlevel", "Qflags", "TAir")


Ta_Hecate$Site <- "Hecate"

RH_Hecate <- df %>% 
  select(Date,RHHecateQ_level,RHHecateQ_flags,RHHecateAvg)
colnames(RH_Hecate) <- c("Date", "Qlevel", "Qflags", "RH")

RH_Hecate$Site <- "Hecate"



#RefStn-------------------------------
Qflags <- "RefStn_Q_flags"
Qlevel <- "RefStn_Q_level"
TAir <- "RefStn_Avg"
RH <- "RefStn_Avg"

Ta_RefStn <- df1 %>% 
  select(Date,TAirRefStnQ_level,TAirRefStnQ_flags,TAirRefStnAvg)
colnames(Ta_RefStn) <- c("Date", "Qlevel", "Qflags", "TAir")


Ta_RefStn$Site <- "RefStn"

RH_RefStn <- df %>% 
  select(Date, RHRefStnQ_level,RHRefStnQ_flags,RHRefStnAvg)
colnames(RH_RefStn) <- c("Date", "Qlevel", "Qflags", "RH")

RH_RefStn$Site <- "RefStn"

#East Buxton-------------------------------
Qflags <- "BuxtonEast_Q_flags"
Qlevel <- "BuxtonEast_Q_level"
TAir <- "BuxtonEast_Avg"
RH <- "BuxtonEast_Avg"

Ta_BuxtonEast <- df %>% 
  select(Date,TAirBuxtonEastQ_level,TAirBuxtonEastQ_flags,TAirBuxtonEastAvg)
colnames(Ta_BuxtonEast) <- c("Date", "Qlevel", "Qflags", "TAir")


Ta_BuxtonEast$Site <- "BuxtonEast"

RH_BuxtonEast <- df %>% 
  select(Date,RHBuxtonEastQ_level, RHBuxtonEastQ_flags,RHBuxtonEastAvg)
colnames(RH_BuxtonEast) <- c("Date", "Qlevel", "Qflags", "RH")

RH_BuxtonEast$Site <- "BuxtonEast"


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


Q_5min<-bind_rows(Q_5min_703, Q_5min_708)


