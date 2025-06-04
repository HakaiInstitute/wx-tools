#WSN626-------------------------------
Qflags <- "WSN626_Q_flags"
Qlevel <- "WSN626_Q_level"
WindDir<-"WindDirWSN626_Avg"
WindSpd<-"WindSpdWSN626_Avg"

WindDir_WSN626 <- df %>% 
  select(Date,WindDirWSN626_Q_level,WindDirWSN626_Q_flags, WindDirWSN626_Avg)
colnames(WindDir_WSN626) <- c("Date", "Qlevel", "Qflags", "WindDir")

WindDir_WSN626$Site <- "WSN626"

WindSpd_WSN626 <- df %>% 
  select(Date,WindSpdWSN626_Q_level,WindSpdWSN626_Q_flags,WindSpdWSN626_Avg)
colnames(WindSpd_WSN626) <- c("Date", "Qlevel", "Qflags", "WindSpd")

WindSpd_WSN626$Site <- "WSN626"

#SSN693-------------------------------
Qflags <- "SSN693_Q_flags"
Qlevel <- "SSN693_Q_level"
WindDir<-"WindDirSSN693_Avg"
WindSpd<-"WindSpdSSN693_Avg"

WindDir_SSN693 <- df %>% 
  select(Date,WindDirSSN693_Q_level,WindDirSSN693_Q_flags,WindDirSSN693_Avg)
colnames(WindDir_SSN693) <- c("Date", "Qlevel", "Qflags", "WindDir")

WindDir_SSN693$Site <- "SSN693"

WindSpd_SSN693 <- df %>% 
  select(Date,WindSpdSSN693_Q_level,WindSpdSSN693_Q_flags,WindSpdSSN693_Avg)
colnames(WindSpd_SSN693) <- c("Date", "Qlevel", "Qflags", "WindSpd")

WindSpd_SSN693$Site <- "SSN693"

#WSN693703-------------------------------
Qflags <- "WSN693703_Q_flags"
Qlevel <- "WSN693703_Q_level"
WindDir<-"WindDirWSN693703_Avg"
WindSpd<-"WindSpdWSN693703_Avg"

WindDir_WSN693703 <- df %>% 
  select(Date,WindDirWSN693_703_Q_level,WindDirWSN693_703_Q_flags,WindDirWSN693_703_Avg)
colnames(WindDir_WSN693703) <- c("Date", "Qlevel", "Qflags", "WindDir")

WindDir_WSN693703$Site <- "WSN693703"

WindSpd_WSN693703 <- df %>% 
  select(Date,WindSpdWSN693_703_Q_level,WindSpdWSN693_703_Q_flags,WindSpdWSN693_703_Avg)
colnames(WindSpd_WSN693703) <- c("Date", "Qlevel", "Qflags", "WindSpd")

WindSpd_WSN693703$Site <- "WSN693703"


#WSN703708-------------------------------
Qflags <- "WSN703_708_Q_flags"
Qlevel <- "WSN703_708_Q_level"
WindDir<-"WindDirWSN703708_Avg"
WindSpd<-"WindSpdWSN703708_Avg"

WindDir_WSN703708 <- df %>% 
  select(Date,WindDirWSN703_708_Q_level,WindDirWSN703_708_Q_flags,WindDirWSN703_708_Avg)
colnames(WindDir_WSN703708) <- c("Date", "Qlevel", "Qflags", "WindDir")

WindDir_WSN703708$Site <- "WSN703708"

WindSpd_WSN703708 <- df %>% 
  select(Date,WindSpdWSN703_708_Q_level,WindSpdWSN703_708_Q_flags,WindSpdWSN703_708_Avg)
colnames(WindSpd_WSN703708) <- c("Date", "Qlevel", "Qflags", "WindSpd")

WindSpd_WSN703708$Site <- "WSN703708"

#WSN8191015-------------------------------
Qflags <- "WSN8191015_Q_flags"
Qlevel <- "WSN8191015_Q_level"
WindDir<-"WindDirWSN8191015_Avg"
WindSpd<-"WindSpdWSN8191015_Avg"

WindDir_WSN8191015 <- df %>% 
  select(Date,WindDirWSN819_1015_Q_level,WindDirWSN819_1015_Q_flags,WindDirWSN819_1015_Avg)
colnames(WindDir_WSN8191015) <- c("Date", "Qlevel", "Qflags", "WindDir")

WindDir_WSN8191015$Site <- "WSN8191015"

WindSpd_WSN8191015 <- df %>% 
  select(Date,WindSpdWSN819_1015_Q_level,WindSpdWSN819_1015_Q_flags,WindSpdWSN819_1015_Avg)
colnames(WindSpd_WSN8191015) <- c("Date", "Qlevel", "Qflags", "WindSpd")

WindSpd_WSN8191015$Site <- "WSN8191015"

#Hecate-------------------------------
Qflags <- "Hecate_Q_flags"
Qlevel <- "Hecate_Q_level"
WindDir<-"WindDirHecate_Avg"
WindSpd<-"WindSpdHecate_Avg"

WindDir_Hecate <- df %>% 
  select(Date,WindDirHecateQ_level,WindDirHecateQ_flags,WindDirHecateAvg)
colnames(WindDir_Hecate) <- c("Date", "Qlevel", "Qflags", "WindDir")

WindDir_Hecate$Site <- "Hecate"

WindSpd_Hecate <- df %>% 
  select(Date,WindSpdHecateQ_level,WindSpdHecateQ_flags,WindSpdHecateAvg)
colnames(WindSpd_Hecate) <- c("Date", "Qlevel", "Qflags", "WindSpd")

WindSpd_Hecate$Site <- "Hecate"

#RefStn-------------------------------
Qflags <- "RefStn_Q_flags"
Qlevel <- "RefStn_Q_level"
WindDir<-"WindDirRefStn_Avg"
WindSpd<-"WindSpdRefStn_Avg"


WindDir_RefStn <- df %>% 
  select(Date,WindDirRefStnQ_level,WindDirRefStnQ_flags,WindDirRefStnAvg)
colnames(WindDir_RefStn) <- c("Date", "Qlevel", "Qflags", "WindDir")

WindDir_RefStn$Site <- "RefStn"

WindSpd_RefStn <- df %>% 
  select(Date,WindSpdRefStnQ_level,WindSpdRefStnQ_flags,WindSpdRefStnAvg)
colnames(WindSpd_RefStn) <- c("Date", "Qlevel", "Qflags", "WindSpd")

WindSpd_RefStn$Site <- "RefStn"

#East Buxton-------------------------------
Qflags <- "BuxtonEast_Q_flags"
Qlevel <- "BuxtonEast_Q_level"
WindDir<-"WindDirBuxtonEast_Avg"
WindSpd<-"WindSpdBuxtonEast_Avg"

WindDir_BuxtonEast <- df %>% 
  select(Date,WindDirBuxtonEastQ_level,WindDirBuxtonEastQ_flags,WindDirBuxtonEastAvg)
colnames(WindDir_BuxtonEast) <- c("Date", "Qlevel", "Qflags", "WindDir")

WindDir_BuxtonEast$Site <- "BuxtonEast"

WindSpd_BuxtonEast <- df %>% 
  select(Date,WindSpdBuxtonEastQ_level,WindSpdBuxtonEastQ_flags,WindSpdBuxtonEastAvg)
colnames(WindSpd_BuxtonEast) <- c("Date", "Qlevel", "Qflags", "WindSpd")

WindSpd_BuxtonEast$Site <- "BuxtonEast"

#Lookout-------------------------------
Qflags <- "Lookout_Q_flags"
Qlevel <- "Lookout_Q_level"
WindDir<-"WindDirLookout_Avg"
WindSpd<-"WindSpdLookout_Avg"

WindDir_Lookout <- df %>% 
  select(Date,WindDirLookoutQ_level,WindDirLookoutQ_flags,WindDirLookoutAvg)
colnames(WindDir_Lookout) <- c("Date", "Qlevel", "Qflags", "WindDir")

WindDir_Lookout$Site <- "Lookout"

WindSpd_Lookout <- df %>% 
  select(Date,WindSpdLookoutQ_level,WindSpdLookoutQ_flags,WindSpdLookoutAvg)
colnames(WindSpd_Lookout) <- c("Date", "Qlevel", "Qflags", "WindSpd")

WindSpd_Lookout$Site <- "Lookout"

#PruthDock-------------------------------
Qflags <- "PruthDock_Q_flags"
Qlevel <- "PruthDock_Q_level"
WindDir<-"WindDirPruthDock_Avg"
WindSpd<-"WindSpdPruthDock_Avg"

WindDir_PruthDock <- df %>% 
  select(Date,WindDirPruthDockQ_level,WindDirPruthDockQ_flags,WindDirPruthDockAvg)
colnames(WindDir_PruthDock) <- c("Date", "Qlevel", "Qflags", "WindDir")

WindDir_PruthDock$Site <- "PruthDock"

WindSpd_PruthDock <- df %>% 
  select(Date,WindSpdPruthDockQ_level,WindSpdPruthDockQ_flags,WindSpdPruthDockAvg)
colnames(WindSpd_PruthDock) <- c("Date", "Qlevel", "Qflags", "WindSpd")

WindSpd_PruthDock$Site <- "PruthDock"

#Ethel-------------------------------

Qflags <- "Ethel_Q_flags"
Qlevel <- "Ethel_Q_level"
WindDir<-"WindDirEthel_Avg"
WindSpd<-"WindSpdEthel_Avg"

WindDir_Ethel <- df %>% 
  select(Date,WindDirEthelQ_level,WindDirEthelQ_flags,WindDirEthelAvg)
colnames(WindDir_Ethel) <- c("Date", "Qlevel", "Qflags", "WindDir")

WindDir_Ethel$Site <- "Ethel"

WindSpd_Ethel <- df %>% 
  select(Date,WindSpdEthelQ_level,WindSpdEthelQ_flags,WindSpdEthelAvg)
colnames(WindSpd_Ethel) <- c("Date", "Qlevel", "Qflags", "WindSpd")

WindSpd_Ethel$Site <- "Ethel"

#Quadra-------------------------------

Qflags <- "Quadra_Q_flags"
Qlevel <- "Quadra_Q_level"
WindDir<-"WindDirQuadra_Avg"
WindSpd<-"WindSpdQuadra_Avg"

WindDir_Quadra <- df %>% 
  select(Date,WindDirQuadraQ_level,WindDirQuadraQ_flags,WindDirQuadraAvg)
colnames(WindDir_Quadra) <- c("Date", "Qlevel", "Qflags", "WindDir")

WindDir_Quadra$Site <- "Quadra"

WindSpd_Quadra <- df %>% 
  select(Date,WindSpdQuadraQ_level,WindSpdQuadraQ_flags,WindSpdQuadraAvg)
colnames(WindSpd_Quadra) <- c("Date", "Qlevel", "Qflags", "WindSpd")

WindSpd_Quadra$Site <- "Quadra"

#Koeye-------------------------------

Qflags <- "Koeye_Q_flags"
Qlevel <- "Koeye_Q_level"
WindDir<-"WindDirKoeye_Avg"
WindSpd<-"WindSpdKoeye_Avg"

WindDir_Koeye <- df %>% 
  select(Date,WindDirKoeyeQ_level,WindDirKoeyeQ_flags,WindDirKoeyeAvg)
colnames(WindDir_Koeye) <- c("Date", "Qlevel", "Qflags", "WindDir")

WindDir_Koeye$Site <- "Koeye"

WindSpd_Koeye <- df %>% 
  select(Date,WindSpdKoeyeQ_level,WindSpdKoeyeQ_flags,WindSpdKoeyeAvg)
colnames(WindSpd_Koeye) <- c("Date", "Qlevel", "Qflags", "WindSpd")

WindSpd_Koeye$Site <- "Koeye"

#Buxton-------------------------------

Qflags <- "Buxton_Q_flags"
Qlevel <- "Buxton_Q_level"
WindDir<-"WindDirBuxton_Avg"
WindSpd<-"WindSpdBuxton_Avg"

WindDir_Buxton <- df %>% 
  select(Date,WindDirBuxtonQ_level,WindDirBuxtonQ_flags,WindDirBuxtonAvg)
colnames(WindDir_Buxton) <- c("Date", "Qlevel", "Qflags", "WindDir")

WindDir_Buxton$Site <- "Buxton"

WindSpd_Buxton <- df %>% 
  select(Date,WindSpdBuxtonQ_level,WindSpdBuxtonQ_flags,WindSpdBuxtonAvg)
colnames(WindSpd_Buxton) <- c("Date", "Qlevel", "Qflags", "WindSpd")

WindSpd_Buxton$Site <- "Buxton"
#------------------------------------------------
Wind_5min <- bind_rows(WindSpd_SSN693,WindSpd_WSN626)
Wind_5min <- bind_rows(Wind_5min, WindSpd_WSN693703)
Wind_5min <- bind_rows(Wind_5min, WindSpd_WSN703708)
Wind_5min <- bind_rows(Wind_5min, WindSpd_WSN8191015)
Wind_5min <- bind_rows(Wind_5min, WindSpd_Hecate)
Wind_5min <- bind_rows(Wind_5min, WindSpd_BuxtonEast)
Wind_5min <- bind_rows(Wind_5min, WindSpd_RefStn)
Wind_5min <- bind_rows(Wind_5min, WindSpd_Ethel)
Wind_5min <- bind_rows(Wind_5min, WindSpd_PruthDock)
Wind_5min <- bind_rows(Wind_5min, WindSpd_Lookout)
Wind_5min <- bind_rows(Wind_5min, WindSpd_Quadra)
Wind_5min <- bind_rows(Wind_5min, WindSpd_Koeye)
Wind_5min <- bind_rows(Wind_5min, WindSpd_Buxton)

write.csv(Wind_5min, "WindSpd_5min_2020.csv")

Wind_5min <- bind_rows(WindDir_SSN693,WindDir_WSN626)
Wind_5min <- bind_rows(Wind_5min, WindDir_WSN693703)
Wind_5min <- bind_rows(Wind_5min, WindDir_WSN703708)
Wind_5min <- bind_rows(Wind_5min, WindDir_WSN8191015)
Wind_5min <- bind_rows(Wind_5min, WindDir_Hecate)
Wind_5min <- bind_rows(Wind_5min, WindDir_BuxtonEast)
Wind_5min <- bind_rows(Wind_5min, WindDir_RefStn)
Wind_5min <- bind_rows(Wind_5min, WindDir_Ethel)
Wind_5min <- bind_rows(Wind_5min, WindDir_PruthDock)
Wind_5min <- bind_rows(Wind_5min, WindDir_Lookout)
Wind_5min <- bind_rows(Wind_5min, WindDir_Quadra)
Wind_5min <- bind_rows(Wind_5min, WindDir_Koeye)
Wind_5min <- bind_rows(Wind_5min, WindDir_Buxton)

write.csv(Wind_5min, "WindDir_5min_2020.csv")

