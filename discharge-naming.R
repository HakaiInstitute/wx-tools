library(tidyverse)
library(lubridate)
library(zoo)
library(dplyr)
library(data.table)

# Read in Q volume and Q rate data (all watersheds), as downloaded from sensor network
# Prepare and edit datasets


# Watershed 626-------------------------------
Qflag <- "Discharge626_Q_flags"
Qlevel<-"Discharge626_Q_level"
Qrate <- "Discharge626"
Qrate_min <- "Discharge626_Min_(95%_CI)"
Qrate_max <- "Discharge626_Max_(95%_CI)"
Qvol <- "DischargeVolume626"
Qvol_min <- "DischargeVolume626_Min_(95%_CI)"
Qvol_max <- "DischargeVolume626_Max_(95%_CI)"

Qrate_5min_626 <- df %>% 
  select(Date, Discharge626_Q_level, Discharge626_Q_flags, Discharge626, `Discharge626_Min_(95%_CI)`, `Discharge626_Max_(95%_CI)`)
colnames(Qrate_5min_626) <- c("Datetime", "Qlevel", "Qflag", "Qrate", "Qrate_min", "Qrate_max")

Qvol_5min_626 <- df %>% 
  select(DischargeVolume626, `DischargeVolume626_Min_(95%_CI)`, `DischargeVolume626_Max_(95%_CI)`)
colnames(Qvol_5min_626) <- c("Qvol", "Qvol_min", "Qvol_max")

Q_5min_626 <- bind_cols(Qrate_5min_626, Qvol_5min_626)
Q_5min_626$Area <- 3.17432237
Q_5min_626 <- Q_5min_626 %>%
  mutate(Qmm = Qvol/(Area*1000)) %>% 
  mutate(Qmm = round(Qmm, digits = 4)) %>% 
  mutate(Qmm_min = Qvol_min/(Area*1000)) %>% 
  mutate(Qmm_min = round(Qmm_min, digits = 4)) %>% 
  mutate(Qmm_max = Qvol_max/(Area*1000)) %>% 
  mutate(Qmm_max = round(Qmm_max, digits = 4)) %>% 
  select(-Area)
Q_5min_626$Watershed <- "WTS626"


# Watershed 703-------------------------------
Qflag <- "Discharge703_Q_flags"
Qlevel<-"Discharge703_Q_level"
Qrate <- "Discharge703"
Qrate_Min <- "Discharge703_Min_(95%_CI)"
Qrate_max <- "Discharge703_Max_(95%_CI)"
Qvol <- "DischargeVolume703"
Qvol_Min <- "DischargeVolume703_Min_(95%_CI)"
Qvol_max <- "DischargeVolume703_Max_(95%_CI)"

Qrate_5min_703 <- df %>% 
  select(Date, Discharge703_Q_level, Discharge703_Q_flags, Discharge703, `Discharge703_Min_(95%_CI)`, `Discharge703_Max_(95%_CI)`)
colnames(Qrate_5min_703) <- c("Datetime", "Qlevel", "Qflag", "Qrate", "Qrate_min", "Qrate_max")

Qvol_5min_703 <- df %>% 
  select(DischargeVolume703, `DischargeVolume703_Min_(95%_CI)`, `DischargeVolume703_Max_(95%_CI)`)
colnames(Qvol_5min_703) <- c("Qvol", "Qvol_min", "Qvol_max")

Q_5min_703 <- bind_cols(Qrate_5min_703, Qvol_5min_703)
Q_5min_703$Area <- 12.79484494
Q_5min_703 <- Q_5min_703 %>%
  mutate(Qmm = Qvol/(Area*1000)) %>% 
  mutate(Qmm = round(Qmm, digits = 4)) %>% 
  mutate(Qmm_min = Qvol_min/(Area*1000)) %>% 
  mutate(Qmm_min = round(Qmm_min, digits = 4)) %>% 
  mutate(Qmm_max = Qvol_max/(Area*1000)) %>% 
  mutate(Qmm_max = round(Qmm_max, digits = 4)) %>% 
  select(-Area)
Q_5min_703$Watershed <- "WTS703"


# Watershed 844-------------------------------
Qflag <- "Discharge844_Q_flags"
Qlevel<-"Discharge844_Q_level"
Qrate <- "Discharge844"
Qrate_Min <- "Discharge844_Min_(95%_CI)"
Qrate_max <- "Discharge844_Max_(95%_CI)"
Qvol <- "DischargeVolume844"
Qvol_Min <- "DischargeVolume844_Min_(95%_CI)"
Qvol_max <- "DischargeVolume844_Max_(95%_CI)"

Qrate_5min_844 <- df %>% 
  select(Date, Discharge844_Q_level, Discharge844_Q_flags, Discharge844, `Discharge844_Min_(95%_CI)`, `Discharge844_Max_(95%_CI)`)
colnames(Qrate_5min_844) <- c("Datetime", "Qlevel", "Qflag", "Qrate", "Qrate_min", "Qrate_max")

Qvol_5min_844 <- df %>% 
  select(DischargeVolume844, `DischargeVolume844_Min_(95%_CI)`, `DischargeVolume844_Max_(95%_CI)`)
colnames(Qvol_5min_844) <- c("Qvol", "Qvol_min", "Qvol_max")

Q_5min_844 <- bind_cols(Qrate_5min_844, Qvol_5min_844)
Q_5min_844$Area <- 12.79484494
Q_5min_844 <- Q_5min_844 %>%
  mutate(Qmm = Qvol/(Area*1000)) %>% 
  mutate(Qmm = round(Qmm, digits = 4)) %>% 
  mutate(Qmm_min = Qvol_min/(Area*1000)) %>% 
  mutate(Qmm_min = round(Qmm_min, digits = 4)) %>% 
  mutate(Qmm_max = Qvol_max/(Area*1000)) %>% 
  mutate(Qmm_max = round(Qmm_max, digits = 4)) %>% 
  select(-Area)
Q_5min_844$Watershed <- "WTS844"

# Watershed 1015-------------------------------
Qflag <- "Discharge1015_Q_flags"
Qlevel<-"Discharge1015_Q_level"
Qrate <- "Discharge1015"
Qrate_Min <- "Discharge1015_Min_(95%_CI)"
Qrate_max <- "Discharge1015_Max_(95%_CI)"
Qvol <- "DischargeVolume1015"
Qvol_Min <- "DischargeVolume1015_Min_(95%_CI)"
Qvol_max <- "DischargeVolume1015_Max_(95%_CI)"

Qrate_5min_1015 <- df %>% 
  select(Date, Discharge1015_Q_level, Discharge1015_Q_flags, Discharge1015, `Discharge1015_Min_(95%_CI)`, `Discharge1015_Max_(95%_CI)`)
colnames(Qrate_5min_1015) <- c("Datetime", "Qlevel", "Qflag", "Qrate", "Qrate_min", "Qrate_max")

Qvol_5min_1015 <- df %>% 
  select(DischargeVolume1015, `DischargeVolume1015_Min_(95%_CI)`, `DischargeVolume1015_Max_(95%_CI)`)
colnames(Qvol_5min_1015) <- c("Qvol", "Qvol_min", "Qvol_max")

Q_5min_1015 <- bind_cols(Qrate_5min_1015, Qvol_5min_1015)
Q_5min_1015$Area <- 12.79484494
Q_5min_1015 <- Q_5min_1015 %>%
  mutate(Qmm = Qvol/(Area*1000)) %>% 
  mutate(Qmm = round(Qmm, digits = 4)) %>% 
  mutate(Qmm_min = Qvol_min/(Area*1000)) %>% 
  mutate(Qmm_min = round(Qmm_min, digits = 4)) %>% 
  mutate(Qmm_max = Qvol_max/(Area*1000)) %>% 
  mutate(Qmm_max = round(Qmm_max, digits = 4)) %>% 
  select(-Area)
Q_5min_1015$Watershed <- "WTS1015"

#--------------------------------------------------------------------------------------------------------------------
Q_5min <- bind_rows(Q_5min_1015,Q_5min_626)
Q_5min <- bind_rows(Q_5min,Q_5min_844)
Q_5min <- bind_rows(Q_5min,Q_5min_703)

write.csv(Q_5min, "Q_5min_2020.csv")
