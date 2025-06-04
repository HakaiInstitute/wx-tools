setwd("C:/Users/Watersheds/Documents/RProjects/Discharge-editing/2019-05-21")

library(tidyverse)
library(lubridate)
library(zoo)
library(dplyr)
library(data.table)

# Read in Q volume and Q rate data (all watersheds), as downloaded from sensor network
# Prepare and edit datasets
Qrate_hourly <- read_csv("data/Qrate_hourly.csv",
                       col_types = cols(
                         Discharge626_Q_flags = col_character(),
                         Discharge626 = col_double(),
                         Discharge626_Min = col_double(),
                         Discharge626_Max = col_double(),
                         Discharge693_Q_flags = col_character(),
                         Discharge693 = col_double(),
                         Discharge693_Min = col_double(),
                         Discharge693_Max = col_double(),
                         Discharge703_Q_flags = col_character(),
                         Discharge703 = col_double(),
                         Discharge703_Min = col_double(),
                         Discharge703_Max = col_double(),
                         Discharge819_Q_flags = col_character(),
                         Discharge819 = col_double(),
                         Discharge819_Min = col_double(),
                         Discharge819_Max = col_double(),
                         Discharge844_Q_flags = col_character(),
                         Discharge844 = col_double(),
                         Discharge844_Min = col_double(),
                         Discharge844_Max = col_double(),
                         Discharge1015_Q_flags = col_character(),
                         Discharge1015 = col_double(),
                         Discharge1015_Min = col_double(),
                         Discharge1015_Max = col_double()))
Qrate_hourly <- Qrate_hourly[-c(2,3,4),]

Qvol_hourly <- read_csv("data/Qvol_hourly.csv",
                      col_types = cols(
                        DischargeVolume626_Q_flags = col_character(),
                        DischargeVolume626 = col_double(),
                        DischargeVolume626_Min = col_double(),
                        DischargeVolume626_Max = col_double(),
                        DischargeVolume693_Q_flags = col_character(),
                        DischargeVolume693 = col_double(),
                        DischargeVolume693_Min = col_double(),
                        DischargeVolume693_Max = col_double(),
                        DischargeVolume703_Q_flags = col_character(),
                        DischargeVolume703 = col_double(),
                        DischargeVolume703_Min = col_double(),
                        DischargeVolume703_Max = col_double(),
                        DischargeVolume819_Q_flags = col_character(),
                        DischargeVolume819 = col_double(),
                        DischargeVolume819_Min = col_double(),
                        DischargeVolume819_Max = col_double(),
                        DischargeVolume844_Q_flags = col_character(),
                        DischargeVolume844 = col_double(),
                        DischargeVolume844_Min = col_double(),
                        DischargeVolume844_Max = col_double(),
                        DischargeVolume1015_Q_flags = col_character(),
                        DischargeVolume1015 = col_double(),
                        DischargeVolume1015_Min = col_double(),
                        DischargeVolume1015_Max = col_double()))      
Qvol_hourly <- Qvol_hourly[-c(2,3,4),]

# Watershed 626-------------------------------
Qflag <- "Discharge626_Q_flags"
Qrate <- "Discharge626"
Qrate_min <- "Discharge626_Min"
Qrate_max <- "Discharge626_Max"
Qvol <- "DischargeVolume626"
Qvol_min <- "DischargeVolume626_Min"
Qvol_max <- "DischargeVolume626_Max"

Qrate_hourly_626 <- Qrate_hourly %>% 
  select(Datetime,Qflag,Qrate,Qrate_min,Qrate_max)
colnames(Qrate_hourly_626) <- c("Datetime", "Qflag", "Qrate", "Qrate_min", "Qrate_max")
Qvol_hourly_626 <- Qvol_hourly %>% 
  select(Qvol,Qvol_min,Qvol_max)
colnames(Qvol_hourly_626) <- c("Qvol", "Qvol_min", "Qvol_max")

Q_hourly_626 <- bind_cols(Qrate_hourly_626, Qvol_hourly_626)
Q_hourly_626$Area <- 3.17432237
Q_hourly_626 <- Q_hourly_626 %>% 
  mutate(Qmm = Qvol/(Area*1000)) %>% 
  mutate(Qmm_min = Qvol_min/(Area*1000)) %>% 
  mutate(Qmm_max = Qvol_max/(Area*1000)) %>% 
  select(-Area)
Q_hourly_626$Watershed <- "WTS626"


# Watershed 693-------------------------------
Qflag <- "Discharge693_Q_flags"
Qrate <- "Discharge693"
Qrate_min <- "Discharge693_Min"
Qrate_max <- "Discharge693_Max"
Qvol <- "DischargeVolume693"
Qvol_min <- "DischargeVolume693_Min"
Qvol_max <- "DischargeVolume693_Max"

Qrate_hourly_693 <- Qrate_hourly %>% 
  select(Datetime,Qflag,Qrate,Qrate_min,Qrate_max)
colnames(Qrate_hourly_693) <- c("Datetime", "Qflag", "Qrate", "Qrate_min", "Qrate_max")
Qvol_hourly_693 <- Qvol_hourly %>% 
  select(Qvol,Qvol_min,Qvol_max)
colnames(Qvol_hourly_693) <- c("Qvol", "Qvol_min", "Qvol_max")

Q_hourly_693 <- bind_cols(Qrate_hourly_693, Qvol_hourly_693)
Q_hourly_693$Area <- 9.27957799
Q_hourly_693 <- Q_hourly_693 %>% 
  mutate(Qmm = Qvol/(Area*1000)) %>% 
  mutate(Qmm_min = Qvol_min/(Area*1000)) %>% 
  mutate(Qmm_max = Qvol_max/(Area*1000)) %>% 
  select(-Area)
Q_hourly_693$Watershed <- "WTS693"

# Watershed 703-------------------------------
Qflag <- "Discharge703_Q_flags"
Qrate <- "Discharge703"
Qrate_min <- "Discharge703_Min"
Qrate_max <- "Discharge703_Max"
Qvol <- "DischargeVolume703"
Qvol_min <- "DischargeVolume703_Min"
Qvol_max <- "DischargeVolume703_Max"

Qrate_hourly_703 <- Qrate_hourly %>% 
  select(Datetime,Qflag,Qrate,Qrate_min,Qrate_max)
colnames(Qrate_hourly_703) <- c("Datetime", "Qflag", "Qrate", "Qrate_min", "Qrate_max")
Qvol_hourly_703 <- Qvol_hourly %>% 
  select(Qvol,Qvol_min,Qvol_max)
colnames(Qvol_hourly_703) <- c("Qvol", "Qvol_min", "Qvol_max")

Q_hourly_703 <- bind_cols(Qrate_hourly_703, Qvol_hourly_703)
Q_hourly_703$Area <- 12.79484494
Q_hourly_703 <- Q_hourly_703 %>% 
  mutate(Qmm = Qvol/(Area*1000)) %>% 
  mutate(Qmm_min = Qvol_min/(Area*1000)) %>% 
  mutate(Qmm_max = Qvol_max/(Area*1000)) %>% 
  select(-Area)
Q_hourly_703$Watershed <- "WTS703"

# Watershed 708-------------------------------
Qflag <- "Discharge708_Q_flags"
Qrate <- "Discharge708"
Qrate_min <- "Discharge708_Min"
Qrate_max <- "Discharge708_Max"
Qvol <- "DischargeVolume708"
Qvol_min <- "DischargeVolume708_Min"
Qvol_max <- "DischargeVolume708_Max"

Qrate_hourly_708 <- Qrate_hourly %>% 
  select(Datetime,Qflag,Qrate,Qrate_min,Qrate_max)
colnames(Qrate_hourly_708) <- c("Datetime", "Qflag", "Qrate", "Qrate_min", "Qrate_max")
Qvol_hourly_708 <- Qvol_hourly %>% 
  select(Qvol,Qvol_min,Qvol_max)
colnames(Qvol_hourly_708) <- c("Qvol", "Qvol_min", "Qvol_max")

Q_hourly_708 <- bind_cols(Qrate_hourly_708, Qvol_hourly_708)
Q_hourly_708$Area <- 7.79354467
Q_hourly_708 <- Q_hourly_708 %>% 
  mutate(Qmm = Qvol/(Area*1000)) %>% 
  mutate(Qmm_min = Qvol_min/(Area*1000)) %>% 
  mutate(Qmm_max = Qvol_max/(Area*1000)) %>% 
  select(-Area)
Q_hourly_708$Watershed <- "WTS708"

# Watershed 819-------------------------------
Qflag <- "Discharge819_Q_flags"
Qrate <- "Discharge819"
Qrate_min <- "Discharge819_Min"
Qrate_max <- "Discharge819_Max"
Qvol <- "DischargeVolume819"
Qvol_min <- "DischargeVolume819_Min"
Qvol_max <- "DischargeVolume819_Max"

Qrate_hourly_819 <- Qrate_hourly %>% 
  select(Datetime,Qflag,Qrate,Qrate_min,Qrate_max)
colnames(Qrate_hourly_819) <- c("Datetime", "Qflag", "Qrate", "Qrate_min", "Qrate_max")
Qvol_hourly_819 <- Qvol_hourly %>% 
  select(Qvol,Qvol_min,Qvol_max)
colnames(Qvol_hourly_819) <- c("Qvol", "Qvol_min", "Qvol_max")

Q_hourly_819 <- bind_cols(Qrate_hourly_819, Qvol_hourly_819)
Q_hourly_819$Area <- 4.81111942
Q_hourly_819 <- Q_hourly_819 %>% 
  mutate(Qmm = Qvol/(Area*1000)) %>% 
  mutate(Qmm_min = Qvol_min/(Area*1000)) %>% 
  mutate(Qmm_max = Qvol_max/(Area*1000)) %>% 
  select(-Area)
Q_hourly_819$Watershed <- "WTS819"

# Watershed 844-------------------------------
Qflag <- "Discharge844_Q_flags"
Qrate <- "Discharge844"
Qrate_min <- "Discharge844_Min"
Qrate_max <- "Discharge844_Max"
Qvol <- "DischargeVolume844"
Qvol_min <- "DischargeVolume844_Min"
Qvol_max <- "DischargeVolume844_Max"

Qrate_hourly_844 <- Qrate_hourly %>% 
  select(Datetime,Qflag,Qrate,Qrate_min,Qrate_max)
colnames(Qrate_hourly_844) <- c("Datetime", "Qflag", "Qrate", "Qrate_min", "Qrate_max")
Qvol_hourly_844 <- Qvol_hourly %>% 
  select(Qvol,Qvol_min,Qvol_max)
colnames(Qvol_hourly_844) <- c("Qvol", "Qvol_min", "Qvol_max")

Q_hourly_844 <- bind_cols(Qrate_hourly_844, Qvol_hourly_844)
Q_hourly_844$Area <- 5.70713684
Q_hourly_844 <- Q_hourly_844 %>% 
  mutate(Qmm = Qvol/(Area*1000)) %>% 
  mutate(Qmm_min = Qvol_min/(Area*1000)) %>% 
  mutate(Qmm_max = Qvol_max/(Area*1000)) %>% 
  select(-Area)
Q_hourly_844$Watershed <- "WTS844"

# Watershed 1015-------------------------------
Qflag <- "Discharge1015_Q_flags"
Qrate <- "Discharge1015"
Qrate_min <- "Discharge1015_Min"
Qrate_max <- "Discharge1015_Max"
Qvol <- "DischargeVolume1015"
Qvol_min <- "DischargeVolume1015_Min"
Qvol_max <- "DischargeVolume1015_Max"

Qrate_hourly_1015 <- Qrate_hourly %>% 
  select(Datetime,Qflag,Qrate,Qrate_min,Qrate_max)
colnames(Qrate_hourly_1015) <- c("Datetime", "Qflag", "Qrate", "Qrate_min", "Qrate_max")
Qvol_hourly_1015 <- Qvol_hourly %>% 
  select(Qvol,Qvol_min,Qvol_max)
colnames(Qvol_hourly_1015) <- c("Qvol", "Qvol_min", "Qvol_max")

Q_hourly_1015 <- bind_cols(Qrate_hourly_1015, Qvol_hourly_1015)
Q_hourly_1015$Area <- 3.32698887
Q_hourly_1015 <- Q_hourly_1015 %>% 
  mutate(Qmm = Qvol/(Area*1000)) %>% 
  mutate(Qmm_min = Qvol_min/(Area*1000)) %>% 
  mutate(Qmm_max = Qvol_max/(Area*1000)) %>% 
  select(-Area)
Q_hourly_1015$Watershed <- "WTS1015"

# merge all hourly files to one file
Q_hourly <- bind_rows(Q_hourly_1015, Q_hourly_626)
Q_hourly <- bind_rows(Q_hourly, Q_hourly_693)
Q_hourly <- bind_rows(Q_hourly, Q_hourly_703)
Q_hourly <- bind_rows(Q_hourly, Q_hourly_708)
Q_hourly <- bind_rows(Q_hourly, Q_hourly_819)
Q_hourly <- bind_rows(Q_hourly, Q_hourly_844)

Q_hourly <- na.omit(Q_hourly)
Q_hourly <- Q_hourly %>%
  mutate(Qrate = round(Qrate, digits = 3)) %>% 
  mutate(Qrate_min = round(Qrate_min, digits = 3)) %>% 
  mutate(Qrate_max = round(Qrate_max, digits = 3)) %>%
  mutate(Qvol = round(Qvol, digits = 3)) %>% 
  mutate(Qvol_min = round(Qvol_min, digits = 3)) %>%
  mutate(Qvol_max = round(Qvol_max, digits = 3)) %>% 
  mutate(Qmm = round(Qmm, digits = 3)) %>%
  mutate(Qmm_min = round(Qmm_min, digits = 3)) %>%
  mutate(Qmm_max = round(Qmm_max, digits = 3))
Q_hourly <- Q_hourly[,c(12,1,2,3,4,5,6,7,8,9,10,11)]

write_csv(Q_hourly, "data_output/Q_hourly.csv")