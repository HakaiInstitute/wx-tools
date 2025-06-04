Watershed <- c("WTS626", "WTS693", "WTS703", "WTS708", "WTS819", "WTS844", "WTS1015")
Area_km2 <- c(3.17432237, 9.27957799, 12.79484494, 7.79354467, 4.81111942, 5.70713684, 3.32698887)

WTS_area <- data.frame(Watershed, Area_km2)


Q_monthly <- merge(Qmonthly_844, WTS_area, by.x = 'Watershed')
Q_monthly <- Q_monthly %>%
  mutate(Qmm = Qvol/(Area_km2*1000)) %>%
  mutate(Qmm = round(Qmm, digits = 0)) %>% 
  mutate(Qmm_min = Qvol_min/(Area_km2*1000)) %>%
  mutate(Qmm_min = round(Qmm_min, digits = 0)) %>% 
  mutate(Qmm_max = Qvol_max/(Area_km2*1000)) %>% 
  mutate(Qmm_max = round(Qmm_max, digits = 0)) %>% 
  mutate(Qrate = round(Qrate, digits = 4)) %>%
  mutate(Qrate_min = round(Qrate_min, digits = 4)) %>% 
  mutate(Qrate_max = round(Qrate_max, digits = 4)) %>% 
  mutate(Qvol = round(Qvol, digits = 0)) %>% 
  mutate(Qvol_min = round(Qvol_min, digits = 0)) %>% 
  mutate(Qvol_max = round(Qvol_max, digits = 0)) %>% 
  select(-Area_km2)
Q_monthly <- Q_monthly[,c(1,2,9,10,11,3,4,5,6,7,8,12,13,14)]

###############################################################################
library(ggplot2)

# Given values
discharge_m3 <- 1514996.52  # Total discharge in cubic meters
precip_mm <- 441  # Total precipitation in mm
area_watershed_m2 <- 5707136.84  # Watershed area in square meters

# Normalize discharge to mm
normalized_discharge_mm <- (discharge_m3 / area_watershed_m2) * 1000

# Prepare data for plotting
data <- data.frame(
  variable = c("Normalized Discharge", "Precipitation"),
  value = c(normalized_discharge_mm, precip_mm)
)

# Plotting the graph
ggplot(data, aes(x = variable, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Normalized Discharge" = "red", "Precipitation" = "blue")) +
  labs(
    x = "Variable",
    y = "Value (mm)",
    title = "Normalized Discharge vs Precipitation for Watershed"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##############################################################################


Qmm<-df %>% 
  mutate(Qmm = 351224.94/Area_km2*1000)

area_wts844<-5707136.840000001 #area in m2

monthly_p<-118.2/1000 #convert to depth in metres

df_long<-df %>%
  pivot_longer(Rain:Qmm,names_to="variable", values_to="mm")

df_long$variable <- str_replace(df_long$variable, "Qmm", "Discharge")

#graphing
qmm_626<-ggplotly(ggplot(df_long, aes(fill=variable, month, mm))+
  geom_bar(position="dodge", stat="identity")+
  scale_fill_manual(values = c("Discharge" = "red", "Rain" = "blue"))+
  theme_bw()+
  theme(
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
     panel.background = element_blank(), axis.line = element_line(colour = "black")
    ) +
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black'))+
  facet_wrap(~watyr)+
    theme(strip.background =element_rect(fill="white"))+
    theme(strip.text = element_text(colour = 'black'))+
  theme(axis.text.x = element_text(angle=90, hjust=1)) +
  labs(x=" ", y= " ", fill="Variable") +
  ggtitle("WTS 626"))

  
 qmm_626





diff<-unit_area_rain-351224
1231600
951558


#############################################################################################################################
#SSN1015

WTS_area_km2<-3.32698887
RainMtd_mm<-596
Qvolume_m3<-1624246.95

Qmm<-(Qvolume_m3/(WTS_area_km2*1000))
#488mm of discharge with 596mm of rain for November 2016

#SSN626
WTS_area_km2<-3.17432237
RainMtd_mm<-536
Qvolume_m3<-1379980.59

Qmm<-(Qvolume_m3/(WTS_area_km2*1000))
#383mm of discharge with 483.3mm of rain for November 2021

#434mm of discharge with 536mm of rain for January 2020

df_month<-df %>%
  group_by(Date= cut(Date, breaks="1 month")) %>% 
  summarise(Discharge= max(DischargeVolume626Mtd, na.rm=TRUE),
            Rain = max(RainSSN626Mtd, na.rm=TRUE))
df_mm<-df_month %>%
  mutate(Qmm=Discharge/(WTS_area_km2*1000))


df_month<-df %>%
  group_by(Date= cut(Date, breaks="1 month")) %>% 
  summarise(Discharge= max(DischargeVolume626Mtd, na.rm=TRUE),
            Rain = max(RainSSN626Mtd, na.rm=TRUE))

df2<-na.omit(df_month)
####################################################################################################3

plot<-ggplot(data = df_wtryr, aes(x = Date)) + 
  geom_bar(aes(fill = Qmm), width = 30) + 
  scale_x_date(date_breaks = "1 month")+ 
               #labels = date_format("%y-%m"),
               #limits = dtLimits)  +
  theme(axis.text.x = element_text(angle = 90, vjust = .5))

str(df_wtryr)
