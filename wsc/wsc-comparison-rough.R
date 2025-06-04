#load packages
lapply(c("tidyverse", "GGally", "lubridate"), library, character.only = TRUE)


#read in wsc and hyac data with na's
wsc_complete_10mins <- read_csv("wsc_complete_10min.csv", 
                                col_types = cols(Date = col_datetime(format = "%Y-%m-%d %H:%M")))

#convert data sets to 10 min logging to be consistent with hyac sensor
df_10<-df %>%
  group_by(Date= cut(Date, breaks="10 min")) %>%   
  summarise(watlev_cc= mean(watlev_cc, na.rm=FALSE),
            watlev_lc= mean(watlev_lc, na.rm=FALSE),
            watlev_rc =mean(watlev_rc, na.rm=FALSE))

#generate complete time-series and fill gaps with NA where applicable
watlev_qual<-lc%>%
  tidyr::complete(Date=seq.POSIXt(min(Date), max(Date), by="5 min"))%>% #little quallicum
  dplyr::mutate(watlev_lc=tidyr::replace_na(watlev_lc,NA))

watlev_carn<-cc%>%
  tidyr::complete(Date=seq.POSIXt(min(Date), max(Date), by="5 min"))%>% #carnation creek
  dplyr::mutate(watlev_cc=tidyr::replace_na(watlev_cc,NA))

watlev_rob<-rc_v2%>%
  tidyr::complete(Date=seq.POSIXt(min(Date), max(Date), by="5 min"))%>% #roberts creek pre and post logging interval shift on 2022-03-14 14:57
  dplyr::mutate(watlev_rc=tidyr::replace_na(watlev_rc,NA))

watlev_hyac<-hc%>%
  tidyr::complete(Date=seq.POSIXt(min(Date), max(Date), by="10 min"))%>% #hyacinthe creek
  dplyr::mutate(`hyac-depth`=tidyr::replace_na(`hyac-depth`,NA))


df_cor = select(df, -c(1))#remove data column to prepare for cor matrix 

#remove extra rows
df_drop<-df[-c(61112:61200),]

df_na<-df_drop %>% 
  summarise(across(everything(), ~ sum(is.na(.)), .names = "{.col}_na.count"))#generate NA count