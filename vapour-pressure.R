#######################################################################################################################
#Vapour Pressure [Dingman, 2002, pp.586]
#######################################################################################################################
#eastbuxton e1x
#hecate h1x
#wsn693703x 
#wsn8191015 
#WSN844
#WSN703708
#wsn703
#ssn693
#ssn1015
#ssn819
#wsn844
#wsn626
#ssn626

#wa1 RH as fraction out of 100
data$wa_Hecate<-(data$RHHecateAvg/100)

#wa2 
#df$wa2<-(df$RH_2/100)

#es2 Saturation vapour pressure second measurement level
#df$es2<-d*exp((17.3*df$T2_Avg)/(df$T2_Avg+273.3))  

#es1 Saturation vapour pressure BuxtonEast
data$es_Hecate<-0.611*exp(17.3*data$TAirHecateAvg/(data$TAirHecateAvg+273.3))

#e2 Vapour pressure second measurement level
#data$e2<-(data$wa2*data$es2)

#e1 Vapour pressure first measurement level
data$e_Hecate<-(data$wa_Hecate*data$es_Hecate)

#ea=Wa*es
#Wa=ea/es

##############################################site specific file#############################################################
hecate<-data %>% 
  select("Date", "Month.x", "Year.x", "WaterYear.x", contains("Hecate"))
write.csv(hecate, "hecate.csv")
BuxtonEast<-data %>% 
  select("Date", "Month.x", "Year.x", "WaterYear.x", contains("BuxtonEast"))
write.csv(BuxtonEast, "BuxtonEast.csv")
WSN693703<-data %>% 
  select("Date", "Month.x", "Year.x", "WaterYear.x", contains("WSN693_703"))
write.csv(WSN693703, "WSN693703.csv")
WSN703708<-data %>% 
  select("Date", "Month.x", "Year.x", "WaterYear.x", contains("WSN703_708"))
write.csv(WSN703708, "WSN703708.csv")
WSN703<-data %>% 
  select("Date", "Month.x", "Year.x", "WaterYear.x", contains("WSN703"))
write.csv(WSN703, "WSN703.csv")
SSN693<-data %>% 
  select("Date", "Month.x", "Year.x", "WaterYear.x", contains("SSN693"))
write.csv(SSN693, "SSN693.csv")
SSN708<-data %>% 
  select("Date", "Month.x", "Year.x", "WaterYear.x", contains("SSN708"))
write.csv(SSN708, "SSN708.csv")
SSN626<-data %>% 
  select("Date", "Month.x", "Year.x", "WaterYear.x", contains("SSN626"))
write.csv(SSN626, "SSN626.csv")
WSN626<-data %>% 
  select("Date", "Month.x", "Year.x", "WaterYear.x", contains("WSN626"))
write.csv(WSN626, "WSN626.csv")
SSN1015<-data %>% 
  select("Date", "Month.x", "Year.x", "WaterYear.x", contains("SSN1015"))
write.csv(SSN1015, "SSN1015.csv")
SSN819<-data %>% 
  select("Date", "Month.x", "Year.x", "WaterYear.x", contains("SSN819"))
write.csv(SSN819, "SSN819.csv")
WSN844<-data %>% 
  select("Date", "Month.x", "Year.x", "WaterYear.x", contains("WSN844"))
write.csv(WSN844, "WSN844.csv")
WSN8191015<-data %>% 
  select("Date", "Month.x", "Year.x", "WaterYear.x", contains("WSN819_1015"))
write.csv(WSN8191015, "WSN8191015.csv")
RefStn<-data %>% 
  select("Date", "Month.x", "Year.x", "WaterYear.x", contains("RefStn"))
write.csv(RefStn, "RefStn.csv")


################################################air temp fill#################################################################
data <- read_csv("BuxtonEast_Ea.csv", col_types = cols(Date = col_datetime(format = "%Y-%m-%d %H:%M:%S"), 
                                              TA2 = col_number(), TA = col_number(), 
                                              RH = col_number()))
WSN8191015<-data %>% 
  mutate(wa1 = if_else(!is.na(RH), RH/100, RH),
         es1 = if_else(!is.na(wa1),0.611*exp((17.3*TAC)/(17.3*TAC+273.3)), wa1),
         ea = if_else(!is.na(es1), wa1*es1, es1))
write.csv(WSN8191015.csv, "WSN8191015.csv_ea.csv", row.names = FALSE)  

###############################################Big merge######################################################################
data <- merge(Hecate, BuxtonEast, by = 'Date')

#300m diff b/w Hecate and BE = 0.01degC so subract 0.02 from Hecate temp 
BE<-data %>% 
  mutate(TAC_C = if_else(is.na(TAC.y), TAC.x-a, TAC.y),
         waC = if_else(!is.na(RH.y), RH.y/100, RH.y),
         esC = 0.611*exp((17.3*TAC_C)/(17.3*TAC_C+273.3)),
         eaC = if_else(esC>0, waC*esC, esC),
         RHF = if_else(!is.na(eaC),(eaC/esC)*100, eaC))
#########################################################################################################################

data <- read_csv("SSN819_ea.csv", col_types = cols(Date = col_datetime(format = "%Y-%m-%d %H:%M:%S"), 
                                                     TA2 = col_skip(), TA = col_skip(), RH = col_skip(), 
                                                     wa1 = col_skip(), es1 = col_skip(), ea = col_skip()))


write.csv(data, "SSN819_TA.csv", row.names = FALSE)  


#Hecate/BuxtonEast
6.5/1000
740-477
a<-2.63*0.006
HE<-data %>% 
  mutate(TAC_C = if_else(is.na(TAC.y), TAC.x-a, TAC.y),
         TAC_C.x= if_else(is.na(TAC.x), TAC.x+a, TAC.x))

data <- merge(WSN693703, BuxtonEast, by = 'Date')
WSN69<-data %>% 
  mutate(TAC_C = if_else(is.na(TAC.y), TAC.x-a, TAC.y),
         TAC_C.x= if_else(is.na(TAC.x), TAC.x+a, TAC.x))

write.csv(HE, "TAgapfill.csv", row.names = FALSE) 



###############################################test function##################################################################

write.csv(dfhecate_ea, "vappres_Test.csv")
         

setwd("C:/Users/emily.haughton/Desktop/TaRH_QC")

my_fun  <- function(file_name){
  file <- read.csv(paste0("GapFill/", file_name[[1]]))
  file$TA_gf <- mutate(TA_gf=if_else(is.na(TA), TA2, TA))
  file$var <- gsub(".csv","",file_name)
}

file_list <- list.files("GapFill/")

dataframe <- ldply(file_list, my_fun)
         
       
