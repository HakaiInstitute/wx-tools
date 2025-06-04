#------------------------------------------------------------------
#WSN626-------------------------------
Qflags <- "WSN626_Q_flags"
Qlevel <- "WSN626_Q_level"
TAir <- "TAirWSN626_Avg"
TAir2 <- "TAirWSN626_2_Avg"
RH <- "RHWSN626_Avg"

Ta_WSN626 <- df %>% 
  select(Date,TAirWSN626_Q_level,TAirWSN626_Q_flags,TAirWSN626_Avg,TAirWSN626_2_Q_level,TAirWSN626_2_Q_flags,TAirWSN626_2_Avg)
colnames(Ta_WSN626) <- c("Date", "Qlevel", "Qflags", "TAir", "Qlevel_2", "Qflags_2","TAir2")

Ta_WSN626$Site <- "WSN626"

RH_WSN626 <- df %>% 
  select(Date,RHWSN626_Q_level,RHWSN626_Q_flags,RHWSN626_Avg)
colnames(RH_WSN626) <- c("Date", "Qlevel", "Qflags", "RH")

RH_WSN626$Site <- "WSN626"

#SSN626-------------------------------
Qflags <- "SSN626_Q_flags"
Qlevel <- "SSN626_Q_level"
TAir <- "TAirSSN626_Avg"
TAir2 <- "TAirSSN626_2_Avg"
RH<- "RHSSN626_Avg"

Ta_SSN626 <- df %>% 
  select(Date,TAirSSN626_Q_level,TAirSSN626_Q_flags,TAirSSN626_Avg,TAirSSN626_2_Q_level,TAirSSN626_2_Q_flags,TAirSSN626_2_Avg)
colnames(Ta_SSN626) <- c("Date", "Qlevel", "Qflags", "TAir", "Qlevel_2", "Qflags_2","TAir2")

Ta_SSN626$Site <- "SSN626"

RH_SSN626 <- df %>% 
  select(Date,RHSSN626_Q_level,RHSSN626_Q_flags,RHSSN626_Avg)
colnames(RH_SSN626) <- c("Date", "Qlevel", "Qflags", "RH")

RH_SSN626$Site <- "SSN626"

#SSN693-------------------------------
Qflags <- "SSN693_Q_flags"
Qlevel <- "SSN693_Q_level"
TAir <- "TAirSSN693_Avg"
TAir2 <- "TAirSSN693_2_Avg"
RH<-"RHSSN693_Avg"

Ta_SSN693 <- df %>% 
  select(Date,TAirSSN693_Q_level,TAirSSN693_Q_flags,TAirSSN693_Avg,TAirSSN693_2_Q_level,TAirSSN693_2_Q_flags,TAirSSN693_2_Avg)
colnames(Ta_SSN693) <- c("Date", "Qlevel", "Qflags", "TAir", "Qlevel_2", "Qflags_2","TAir2")


Ta_SSN693$Site <- "SSN693"

RH_SSN693 <- df %>% 
  select(Date,RHSSN693_Q_level,RHSSN693_Q_flags,RHSSN693_Avg)
colnames(RH_SSN693) <- c("Date", "Qlevel", "Qflags", "RH")

RH_SSN693$Site <- "SSN693"

#WSN693703-------------------------------
Qflags <- "WSN693703_Q_flags"
Qlevel <- "WSN693703_Q_level"
TAir <- "TAirWSN693703_Avg"
TAir2 <- "TAirWSN693_703_2_Avg"
RH<-"RHWSN693703_Avg"

Ta_WSN693703 <- df %>% 
  select(Date,TAirWSN693_703_Q_level,TAirWSN693_703_Q_flags,TAirWSN693_703_Avg,TAirWSN693_703_2_Q_level,TAirWSN693_703_2_Q_flags,TAirWSN693_703_2_Avg)
colnames(Ta_WSN693703) <- c("Date", "Qlevel", "Qflags", "TAir", "Qlevel_2", "Qflags_2","TAir2")


Ta_WSN693703$Site <- "WSN693703"

RH_WSN693703 <- df %>% 
  select(Date,RHWSN693_703_Q_level,RHWSN693_703_Q_flags,RHWSN693_703_Avg)
colnames(RH_WSN693703) <- c("Date", "Qlevel", "Qflags", "RH")

RH_WSN693703$Site <- "WSN693703"

#WSN703-------------------------------
Qflags <- "WSN703_Q_flags"
Qlevel <- "WSN703_Q_level"
TAir <- "TAirWSN703_Avg"
TAir2 <- "TAirWSN703_2_Avg"
RH <- "RHWSN703_Avg"

Ta_WSN703 <- df %>% 
  select(Date,TAirWSN703_Q_level,TAirWSN703_Q_flags,TAirWSN703_Avg,TAirWSN703_2_Q_level,TAirWSN703_2_Q_flags,TAirWSN703_2_Avg)
colnames(Ta_WSN703) <- c("Date", "Qlevel", "Qflags", "TAir", "Qlevel_2", "Qflags_2","TAir2")


Ta_WSN703$Site <- "WSN703"

RH_WSN703 <- df %>% 
  select(Date,RHWSN703_Q_level,RHWSN703_Q_flags,RHWSN703_Avg)
colnames(RH_WSN703) <- c("Date", "Qlevel", "Qflags", "RH")

RH_WSN703$Site <- "WSN703"

#WSN703708-------------------------------
Qflags <- "WSN703_708_Q_flags"
Qlevel <- "WSN703_708_Q_level"
TAir <- "TAirWSN703_708_Avg"
TAir2 <- "TAirWSN703_708_2_Avg"
RH <- "RHWSN703708_Avg"

Ta_WSN703708 <- df %>% 
  select(Date,TAirWSN703_708_Q_level,TAirWSN703_708_Q_flags,TAirWSN703_708_Avg,TAirWSN703_708_2_Q_level,TAirWSN703_708_2_Q_flags,TAirWSN703_708_2_Avg)
colnames(Ta_WSN703708) <- c("Date", "Qlevel", "Qflags", "TAir", "Qlevel_2", "Qflags_2","TAir2")


Ta_WSN703708$Site <- "WSN703_708"

RH_WSN703708 <- df %>% 
  select(Date,RHWSN703_708_Q_level,RHWSN703_708_Q_flags,RHWSN703_708_Avg)
colnames(RH_WSN703708) <- c("Date", "Qlevel", "Qflags", "RH")

RH_WSN703708$Site <- "WSN703708"

#SSN708-------------------------------
Qflags <- "SSN708_Q_flags"
Qlevel <- "SSN708_Q_level"
TAir <- "TAirSSN708_Avg"
TAir2<-"TAirSSN708_2_Avg"
RH <- "RHSSN708_Avg"

Ta_SSN708 <- df %>% 
  select(Date, TAirSSN708_Q_level,TAirSSN708_Q_flags,TAirSSN708_Avg,TAirSSN708_2_Q_level,TAirSSN708_2_Q_flags,TAirSSN708_2_Avg)
colnames(Ta_SSN708) <- c("Date", "Qlevel", "Qflags", "TAir", "Qlevel_2", "Qflags_2","TAir2")


Ta_SSN708$Site <- "SSN708"

RH_SSN708 <- df %>% 
  select(Date,RHSSN708_Q_level,RHSSN708_Q_flags,RHSSN708_Avg)
colnames(RH_SSN708) <- c("Date", "Qlevel", "Qflags", "RH")

RH_SSN708$Site <- "SSN708"

#SSN819-------------------------------
Qflags <- "SSN819_Q_flags"
Qlevel <- "SSN819_Q_level"
TAir <- "TAirSSN819_Avg"
TAir2 <- "TAirSSN819_2_Avg"
RH <- "RHSSN819_Avg"

Ta_SSN819 <- df %>% 
  select(Date,TAirSSN819_Q_level,TAirSSN819_Q_flags,TAirSSN819_Avg,TAirSSN819_2_Q_level,TAirSSN819_2_Q_flags,TAirSSN819_2_Avg)
colnames(Ta_SSN819) <- c("Date", "Qlevel", "Qflags", "TAir", "Qlevel_2", "Qflags_2","TAir2")


Ta_SSN819$Site <- "SSN819"

RH_SSN819 <- df %>% 
  select(Date,RHSSN819_Q_level,RHSSN819_Q_flags,RHSSN819_Avg)
colnames(RH_SSN819) <- c("Date", "Qlevel", "Qflags", "RH")

RH_SSN819$Site <- "SSN819"

#WSN8191015-------------------------------
Qflags <- "WSN8191015_Q_flags"
Qlevel <- "WSN8191015_Q_level"
TAir <- "TAirWSN8191015_Avg"
TAir2 <- "TAirWSN8191015_2_Avg"
RH <- "RHWSN8191015_Avg"

Ta_WSN8191015 <- df %>% 
  select(Date,TAirWSN819_1015_Q_level,TAirWSN819_1015_Q_flags,TAirWSN819_1015_Avg,TAirWSN819_1015_2_Q_level,TAirWSN819_1015_2_Q_flags,TAirWSN819_1015_2_Avg)
colnames(Ta_WSN8191015) <- c("Date", "Qlevel", "Qflags", "TAir", "Qlevel_2", "Qflags_2","TAir2")


Ta_WSN8191015$Site <- "WSN8191015"

RH_WSN8191015 <- df %>% 
  select(Date, RHWSN819_1015_Q_level,RHWSN819_1015_Q_flags,RHWSN819_1015_Avg)
colnames(RH_WSN8191015) <- c("Date", "Qlevel", "Qflags", "RH")

RH_WSN8191015$Site <- "WSN8191015"

#WSN844-------------------------------
Qflags <- "WSN844_Q_flags"
Qlevel <- "WSN844_Q_level"
TAir <- "TAirWSN844_Avg"
TAir2 <- "TAirWSN844_2_Avg"
RH <- "RHWSN844_Avg"

Ta_WSN844 <- df %>% 
  select(Date,TAirWSN844_Q_level,TAirWSN844_Q_flags,TAirWSN844_Avg,TAirWSN844_2_Q_level,TAirWSN844_2_Q_flags,TAirWSN844_2_Avg)
colnames(Ta_WSN844) <- c("Date", "Qlevel", "Qflags", "TAir", "Qlevel_2", "Qflags_2","TAir2")


Ta_WSN844$Site <- "WSN844"

RH_WSN844 <- df %>% 
 select(Date,Qlevel_RHWSN844,Qflags_RHWSN844,RHWSN844_Avg)
colnames(RH_WSN844) <- c("Date", "Qlevel", "Qflags", "RH")

RH_WSN844$Site <- "WSN844"

#SSN1015-------------------------------
Qflags <- "SSN1015_Q_flags"
Qlevel <- "SSN1015_Q_level"
TAir <- "TAirSSN1015_Avg"
TAir2 <- "TAirSSN1015_2_Avg"
RH <- "RHSSN1015_Avg"

Ta_SSN1015 <- df %>% 
  select(Date,TAirSSN1015_Q_level,TAirSSN1015_Q_flags,TAirSSN1015_Avg,TAirSSN1015_2_Q_level,TAirSSN1015_2_Q_flags,TAirSSN1015_2_Avg)
colnames(Ta_SSN1015) <- c("Date", "Qlevel", "Qflags", "TAir", "Qlevel_2", "Qflags_2","TAir2")


Ta_SSN1015$Site <- "SSN1015"

RH_SSN1015 <- df %>% 
  select(Date,RHSSN1015_Q_level,RHSSN1015_Q_flags,RHSSN1015_Avg)
colnames(RH_SSN1015) <- c("Date", "Qlevel", "Qflags", "RH")

RH_SSN1015$Site <- "SSN1015"


#Hecate-------------------------------
Qflags <- "Hecate_Q_flags"
Qlevel <- "Hecate_Q_level"
TAir <- "Hecate_Avg"
TAir2 <- "Hecate_2_Avg"
RH <- "Hecate_Avg"

Ta_Hecate <- df %>% 
  select(Date,TAirHecateQ_level,TAirHecateQ_flags,TAirHecateAvg,TAirHecate2_Q_level,TAirHecate2_Q_flags,TAirHecate2_Avg)
colnames(Ta_Hecate) <- c("Date", "Qlevel", "Qflags", "TAir", "Qlevel_2", "Qflags_2","TAir2")


Ta_Hecate$Site <- "Hecate"

RH_Hecate <- df %>% 
  select(Date,RHHecateQ_level,RHHecateQ_flags,RHHecateAvg)
colnames(RH_Hecate) <- c("Date", "Qlevel", "Qflags", "RH")

RH_Hecate$Site <- "Hecate"



#RefStn-------------------------------
Qflags <- "RefStn_Q_flags"
Qlevel <- "RefStn_Q_level"
TAir <- "RefStn_Avg"
TAir2 <- "RefStn2_Avg"
RH <- "RefStn_Avg"

Ta_RefStn <- df %>% 
  select(Date,TAirRefStnQ_level,TAirRefStnQ_flags,TAirRefStnAvg,TAirRefStn2_Q_level,TAirRefStn2_Q_flags,TAirRefStn2_Avg)
colnames(Ta_RefStn) <- c("Date", "Qlevel", "Qflags", "TAir", "Qlevel_2", "Qflags_2","TAir2")


Ta_RefStn$Site <- "RefStn"

RH_RefStn <- df %>% 
  select(Date, RHRefStnQ_level,RHRefStnQ_flags,RHRefStnAvg)
colnames(RH_RefStn) <- c("Date", "Qlevel", "Qflags", "RH")

RH_RefStn$Site <- "RefStn"

#East Buxton-------------------------------
Qflags <- "BuxtonEast_Q_flags"
Qlevel <- "BuxtonEast_Q_level"
TAir <- "BuxtonEast_Avg"
TAir2 <- "BuxtonEast2_Avg"
RH <- "BuxtonEast_Avg"

Ta_BuxtonEast <- df %>% 
  select(Date,TAirBuxtonEastQ_level,TAirBuxtonEastQ_flags,TAirBuxtonEastAvg,TAirBuxtonEast2_Q_level,TAirBuxtonEast2_Q_flags,TAirBuxtonEast2_Avg)
colnames(Ta_BuxtonEast) <- c("Date", "Qlevel", "Qflags", "TAir", "Qlevel_2", "Qflags_2","TAir2")


Ta_BuxtonEast$Site <- "BuxtonEast"

RH_BuxtonEast <- df %>% 
  select(Date,RHBuxtonEastQ_level, RHBuxtonEastQ_flags,RHBuxtonEastAvg)
colnames(RH_BuxtonEast) <- c("Date", "Qlevel", "Qflags", "RH")

RH_BuxtonEast$Site <- "BuxtonEast"

#Buxton-------------------------------
Qflags <- "Buxton_Q_flags"
Qlevel <- "Buxton_Q_level"
TAir <- "Buxton_Avg"
TAir2 <- "Buxton2_Avg"
RH <- "Buxton_Avg"

Ta_Buxton<- df %>% 
  select(Date,TAirBuxton1_Q_level,TAirBuxton1_Q_flags,TAirBuxton1_Avg,TAirBuxton2_Q_level,TAirBuxton2_Q_flags,TAirBuxton2_Avg)
colnames(Ta_Buxton) <- c("Date", "Qlevel", "Qflags", "TAir", "Qlevel_2", "Qflags_2","TAir2")


Ta_Buxton$Site <- "Buxton"

RH_Buxton <- df %>% 
  select(Date,RHBuxtonQ_level, RHBuxtonQ_flags,RHBuxtonAvg)
colnames(RH_Buxton) <- c("Date", "Qlevel", "Qflags", "RH")

RH_Buxton$Site <- "Buxton"

#Lookout-------------------------------
Qflags <- "Lookout_Q_flags"
Qlevel <- "Lookout_Q_level"
TAir <- "Lookout_Avg"
TAir2 <- "Lookout2_Avg"
RH <- "Lookout_Avg"

Ta_Lookout<- df %>% 
  select(Date,TAirLookout1_Q_level,TAirLookout1_Q_flags,TAirLookout1_Avg,TAirLookout2_Q_level,TAirLookout2_Q_flags,TAirLookout2_Avg)
colnames(Ta_Lookout) <- c("Date", "Qlevel", "Qflags", "TAir", "Qlevel_2", "Qflags_2","TAir2")


Ta_Lookout$Site <- "Lookout"

RH_Lookout <- df %>% 
  select(Date,RHLookoutQ_level, RHLookoutQ_flags,RHLookoutAvg)
colnames(RH_Lookout) <- c("Date", "Qlevel", "Qflags", "RH")

RH_Lookout$Site <- "Lookout"

#PruthDock-------------------------------
Qflags <- "PruthDock_Q_flags"
Qlevel <- "PruthDock_Q_level"
TAir <- "PruthDock_Avg"
TAir2 <- "PruthDock2_Avg"
RH <- "PruthDock_Avg"

Ta_PruthDock<- df %>% 
  select(Date,TAirPruthDockQ_level,TAirPruthDockQ_flags,TAirPruthDockAvg,TAirPruthDock2_Q_level,TAirPruthDock2_Q_flags,TAirPruthDock2_Avg)
colnames(Ta_PruthDock) <- c("Date", "Qlevel", "Qflags", "TAir", "Qlevel_2", "Qflags_2","TAir2")


Ta_PruthDock$Site <- "PruthDock"

RH_PruthDock <- df %>% 
  select(Date,RHPruthDockQ_level, RHPruthDockQ_flags,RHPruthDockAvg)
colnames(RH_PruthDock) <- c("Date", "Qlevel", "Qflags", "RH")

RH_PruthDock$Site <- "PruthDock"

#Ethel-------------------------------
Qflags <- "Ethel_Q_flags"
Qlevel <- "Ethel_Q_level"
TAir <- "Ethel_Avg"
TAir2 <- "Ethel2_Avg"
RH <- "Ethel_Avg"

Ta_Ethel<- df %>% 
  select(Date,TAirEthel1_Q_level,TAirEthel1_Q_flags,TAirEthel1_Avg,TAirEthel2_Q_level,TAirEthel2_Q_flags,TAirEthel2_Avg)
colnames(Ta_Ethel) <- c("Date", "Qlevel", "Qflags", "TAir", "Qlevel_2", "Qflags_2","TAir2")


Ta_Ethel$Site <- "Ethel"

RH_Ethel <- df %>% 
  select(Date,RHEthelQ_level, RHEthelQ_flags,RHEthelAvg)
colnames(RH_Ethel) <- c("Date", "Qlevel", "Qflags", "RH")

RH_Ethel$Site <- "Ethel"

#Koeye-------------------------------
Qflags <- "Koeye_Q_flags"
Qlevel <- "Koeye_Q_level"
TAir <- "Koeye_Avg"
TAir2 <- "Koeye2_Avg"
RH <- "Koeye_Avg"

Ta_Koeye<- df %>% 
  select(Date,TAirKoeye1_Q_level,TAirKoeye1_Q_flags,TAirKoeye1_Avg,TAirKoeye2_Q_level,TAirKoeye2_Q_flags,TAirKoeye2_Avg)
colnames(Ta_Koeye) <- c("Date", "Qlevel", "Qflags", "TAir", "Qlevel_2", "Qflags_2","TAir2")


Ta_Koeye$Site <- "Koeye"

RH_Koeye <- df %>% 
  select(Date,RHKoeyeQ_level, RHKoeyeQ_flags,RHKoeyeAvg)
colnames(RH_Koeye) <- c("Date", "Qlevel", "Qflags", "RH")

RH_Koeye$Site <- "Koeye"

#Quadra-------------------------------
Qflags <- "Quadra_Q_flags"
Qlevel <- "Quadra_Q_level"
TAir <- "Quadra_Avg"
TAir2 <- "Quadra2_Avg"
RH <- "Quadra_Avg"

Ta_Quadra<- df %>% 
  select(Date,TAirQuadra1_Q_level,TAirQuadra1_Q_flags,TAirQuadra1_Avg,TAirQuadra2_Q_level,TAirQuadra2_Q_flags,TAirQuadra2_Avg)
colnames(Ta_Quadra) <- c("Date", "Qlevel", "Qflags", "TAir", "Qlevel_2", "Qflags_2","TAir2")


Ta_Quadra$Site <- "Quadra"

RH_Quadra <- df %>% 
  select(Date,RHQuadraQ_level, RHQuadraQ_flags,RHQuadraAvg)
colnames(RH_Quadra) <- c("Date", "Qlevel", "Qflags", "RH")

RH_Quadra$Site <- "Quadra"

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
Ta_5min <- bind_rows(Ta_5min, Ta_PruthDock)
Ta_5min <- bind_rows(Ta_5min, Ta_Lookout)
Ta_5min <- bind_rows(Ta_5min, Ta_Ethel)
Ta_5min <- bind_rows(Ta_5min, Ta_Koeye)
Ta_5min <- bind_rows(Ta_5min, Ta_Quadra)
Ta_5min <- bind_rows(Ta_5min, Ta_Buxton)

write.csv(Ta_5min, "ta_5min_2022.csv")

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
RH_5min <- bind_rows(RH_5min, RH_Lookout)
RH_5min <- bind_rows(RH_5min, RH_PruthDock)
RH_5min <- bind_rows(RH_5min, RH_Buxton)
RH_5min <- bind_rows(RH_5min, RH_Ethel)
RH_5min <- bind_rows(RH_5min, RH_Koeye)
RH_5min <- bind_rows(RH_5min, RH_Quadra)

write.csv(RH_5min, "RH_5min_2022.csv")

TaRH_5min<-bind_rows(Ta_5min, RH_5min)


