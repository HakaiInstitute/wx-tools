#------------------------------------------------------------------
#WSN626-------------------------------

Qflags <- "WSN626_Q_flags"
Qlevel <- "WSN626_Q_level"
Rain<-"RainWSN626"

Rain_WSN626 <- df %>% 
  select(Date, RainWSN626_Q_level, RainWSN626_Q_flags, RainWSN626)
colnames( Rain_WSN626) <- c("Date", "Qlevel", "Qflags", "Rain")

Rain_WSN626$Site <- "WSN626"

#SSN626-------------------------------

Qflags <- "SSN626_Q_flags"
Qlevel <- "SSN626_Q_level"
Rain<-"RainSSN626"

Rain_SSN626 <- df %>% 
  select(Date, RainSSN626_Q_level, RainSSN626_Q_flags, RainSSN626)
colnames( Rain_SSN626) <- c("Date", "Qlevel", "Qflags", "Rain")

Rain_SSN626$Site <- "SSN626"

#SSN693-------------------------------
Qflags <- "SSN693_Q_flags"
Qlevel <- "SSN693_Q_level"
Rain<-"RainSSN693"

Rain_SSN693 <- df %>% 
  select(Date, RainSSN693_Q_level, RainSSN693_Q_flags, RainSSN693)
colnames( Rain_SSN693) <- c("Date", "Qlevel", "Qflags", "Rain")

Rain_SSN693$Site <- "SSN693"

#WSN693703-------------------------------

Qflags <- "WSN693703_Q_flags"
Qlevel <- "WSN693703_Q_level"
Rain<-"RainWSN693_703"

Rain_WSN693_703 <- df %>% 
  select(Date, RainWSN693_703_Q_level, RainWSN693_703_Q_flags, RainWSN693_703)
colnames( Rain_WSN693_703) <- c("Date", "Qlevel", "Qflags", "Rain")

Rain_WSN693_703$Site <- "WSN693_703"

#WSN703-------------------------------
Qflags <- "WSN703_Q_flags"
Qlevel <- "WSN703_Q_level"
Rain<- "RainWSN703"

Rain_WSN703 <- df %>% 
  select(Date, RainWSN703_Q_level, RainWSN703_Q_flags, RainWSN703)
colnames( Rain_WSN703) <- c("Date", "Qlevel", "Qflags", "Rain")

Rain_WSN703$Site <- "WSN703"

#WSN703708-------------------------------
Qflags <- "WSN703_708_Q_flags"
Qlevel <- "WSN703_708_Q_level"
RainWSN703_708<-"RainWSN703_708"

Rain_WSN703_708 <- df %>% 
  select(Date, RainWSN703_708_Q_level, RainWSN703_708_Q_flags, RainWSN703_708)
colnames( Rain_WSN703_708) <- c("Date", "Qlevel", "Qflags", "Rain")

Rain_WSN703_708$Site <- "WSN703_708"

#SSN708-------------------------------
Qflags <- "SSN708_Q_flags"
Qlevel <- "SSN708_Q_level"
RainSSN708<- "RainSSN708"

Rain_SSN708 <- df %>% 
  select(Date, RainSSN708_Q_level, RainSSN708_Q_flags, RainSSN708)
colnames( Rain_SSN708) <- c("Date", "Qlevel", "Qflags", "Rain")

Rain_SSN708$Site <- "SSN708"

#SSN819-------------------------------
Qflags <- "SSN819_Q_flags"
Qlevel <- "SSN819_Q_level"
RainSSN819<- "RainSSN819"

Rain_SSN819 <- df %>% 
  select(Date, RainSSN819_Q_level, RainSSN819_Q_flags, RainSSN819)
colnames( Rain_SSN819) <- c("Date", "Qlevel", "Qflags", "Rain")

Rain_SSN819$Site <- "SSN819"

#WSN8191015-------------------------------
Qflags <- "WSN8191015_Q_flags"
Qlevel <- "WSN8191015_Q_level"
RainWSN819_1015<- "RainWSN819_1015"

Rain_WSN819_1015 <- df %>% 
  select(Date, RainWSN819_1015_Q_level, RainWSN819_1015_Q_flags, RainWSN819_1015)
colnames( Rain_WSN819_1015) <- c("Date", "Qlevel", "Qflags", "Rain")

Rain_WSN819_1015$Site <- "WSN819_1015"

#WSN844-------------------------------
Qflags <- "WSN844_Q_flags"
Qlevel <- "WSN844_Q_level"
RainWSN844<- "RainWSN844"

Rain_WSN844 <- df %>% 
  select(Date, RainWSN844_Q_level, RainWSN844_Q_flags, RainWSN844)
colnames( Rain_WSN844) <- c("Date", "Qlevel", "Qflags", "Rain")

Rain_WSN844$Site <- "WSN844"
#SSN1015-------------------------------
Qflags <- "SSN1015_Q_flags"
Qlevel <- "SSN1015_Q_level"
RainSSN1015<- "RainSSN1015"

Rain_SSN1015 <- df %>% 
  select(Date, RainSSN1015_Q_level, RainSSN1015_Q_flags, RainSSN1015)
colnames( Rain_SSN1015) <- c("Date", "Qlevel", "Qflags", "Rain")

Rain_SSN1015$Site <- "SSN1015"

#Hecate-------------------------------
Qflags <- "Hecate_Q_flags"
Qlevel <- "Hecate_Q_level"
RainHecate<- "RainHecate"

Rain_Hecate <- df %>% 
  select(Date, RainHecateQ_level, RainHecateQ_flags, RainHecate)
colnames( Rain_Hecate) <- c("Date", "Qlevel", "Qflags", "Rain")

Rain_Hecate$Site <- "Hecate"


#RefStn-------------------------------
Qflags <- "RefStn_Q_flags"
Qlevel <- "RefStn_Q_level"
RainRefStn<- "RainRefStn"

Rain_RefStn <- df %>% 
  select(Date, RainRefStnQ_level, RainRefStnQ_flags, RainRefStn)
colnames( Rain_RefStn) <- c("Date", "Qlevel", "Qflags", "Rain")

Rain_RefStn$Site <- "RefStn"

#East Buxton-------------------------------
Qflags <- "BuxtonEast_Q_flags"
Qlevel <- "BuxtonEast_Q_level"
RainBuxtonEast<- "RainBuxtonEast"

Rain_BuxtonEast <- df %>% 
  select(Date, RainBuxtonEastQ_level, RainBuxtonEastQ_flags, RainBuxtonEast)
colnames( Rain_BuxtonEast) <- c("Date", "Qlevel", "Qflags", "Rain")

Rain_BuxtonEast$Site <- "BuxtonEast"

#Lookout-------------------------------
Qflags <- "Lookout_Q_flags"
Qlevel <- "Lookout_Q_level"
RainLookout<- "RainLookout"

Rain_Lookout <- df %>% 
  select(Date, RainLookoutQ_level, RainLookoutQ_flags, RainLookout)
colnames( Rain_Lookout) <- c("Date", "Qlevel", "Qflags", "Rain")

Rain_Lookout$Site <- "Lookout"

#PruthDock-------------------------------
Qflags <- "Pruth_Q_flags"
Qlevel <- "Pruth_Q_level"
RainPruth<- "RainPruth"

Rain_Pruth <- df %>% 
  select(Date, RainPruthQ_level, RainPruthQ_flags, RainPruth)
colnames( Rain_Pruth) <- c("Date", "Qlevel", "Qflags", "Rain")

Rain_Pruth$Site <- "Pruth"

#Ethel-------------------------------
Qflags <- "Ethel_Q_flags"
Qlevel <- "Ethel_Q_level"
RainEthel<- "RainEthel"

Rain_Ethel <- df %>% 
  select(Date, RainEthelQ_level, RainEthelQ_flags, RainEthel)
colnames( Rain_Ethel) <- c("Date", "Qlevel", "Qflags", "Rain")

Rain_Ethel$Site <- "Ethel"

#Koeye-------------------------------
Qflags <- "Koeye_Q_flags"
Qlevel <- "Koeye_Q_level"
RainKoeye<- "RainKoeye"

Rain_Koeye <- df %>% 
  select(Date, RainKoeyeQ_level, RainKoeyeQ_flags, RainKoeye)
colnames( Rain_Koeye) <- c("Date", "Qlevel", "Qflags", "Rain")

Rain_Koeye$Site <- "Koeye"

#Quadra-------------------------------
Qflags <- "Quadra_Q_flags"
Qlevel <- "Quadra_Q_level"
RainQuadra<- "RainQuadra"

Rain_Quadra <- df %>% 
  select(Date, RainQuadraQ_level, RainQuadraQ_flags, RainQuadra)
colnames( Rain_Quadra) <- c("Date", "Qlevel", "Qflags", "Rain")

Rain_Quadra$Site <- "Quadra"

#######5min
# merge all hourly files to one file
Rain_5min <- bind_rows(Rain_SSN626, Rain_WSN626)
Rain_5min <- bind_rows(Rain_5min, Rain_SSN693)
Rain_5min <- bind_rows(Rain_5min, Rain_WSN693_703)
Rain_5min <- bind_rows(Rain_5min, Rain_WSN703)
Rain_5min <- bind_rows(Rain_5min, Rain_SSN708)
Rain_5min <- bind_rows(Rain_5min, Rain_WSN703_708)
Rain_5min <- bind_rows(Rain_5min, Rain_WSN819_1015)
Rain_5min <- bind_rows(Rain_5min, Rain_SSN819)
Rain_5min <- bind_rows(Rain_5min, Rain_SSN1015)
Rain_5min <- bind_rows(Rain_5min, Rain_WSN844)
Rain_5min <- bind_rows(Rain_5min, Rain_Hecate)
Rain_5min <- bind_rows(Rain_5min, Rain_BuxtonEast)
Rain_5min <- bind_rows(Rain_5min, Rain_RefStn)
Rain_5min <- bind_rows(Rain_5min, Rain_Pruth)
Rain_5min <- bind_rows(Rain_5min, Rain_Lookout)
Rain_5min <- bind_rows(Rain_5min, Rain_Ethel)
Rain_5min <- bind_rows(Rain_5min, Rain_Koeye)
Rain_5min <- bind_rows(Rain_5min, Rain_Quadra)



write.csv(Rain_5min, "Rain_5min_2020.csv")

TaRH_5min<-bind_rows(Ta_5min, RH_5min)


