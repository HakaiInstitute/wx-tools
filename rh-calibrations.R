#test figure
df_plot <- data_trim %>% 
  select(Date,  WaterYear, RHWSN844_Avg, RHWSN819_1015_Avg, RHHecateAvg, RHRefStnAvg, RHWSN626_Avg, RHWSN703_Avg ) %>% 
  pivot_longer(RHWSN844_Avg:RHWSN703_Avg) 

plot<-df_plot %>%
  ggplot(aes(x=Date, y=value))+
  geom_point(alpha=0.5)+
  facet_wrap(~name, ncol=1)+
  theme(legend.position="none")+
  labs(x="Date")
plot(plot)

require(dplyr)

TaRH_16_09$Date<-as.POSIXct(TaRH_16_09$Date,format="%Y-%m-%d hh:mm:ss")
str(TaRH_16_09)

b5<-TaRH_20_09 %>%
  group_by(Date= cut(Date, breaks="5 min")) %>%
  summarize (
    Temp=mean(Temp),
    RH=mean(RH))
write.csv(b1,"aggregatedWSN844_calib.csv")

merged<-merge(df_hobo, TaRH_network)

#Test for equal variances############################################################################################################

#t = t-value
#X1 = average of sample
#s = std deviation of sample
#n = number of values in sample
#xs = stdev


#mean of each column
u_sensor<-mean(df$SensorAvg)
u_NIST<-mean(df$NistAvg)

#calculate diff between mean and each obs -- square it 
df$sensor_var<-(df$SensorAvg-u_sensor)^2
df$NIST_var<-(df$NistAvg-u_NIST)^2

#calculate variance
sensor_variance<-mean(df$sensor_var)
NIST_variance<-mean(df$NIST_var)

#calculate population stdev
sensor_pstdev<-sqrt(sensor_variance)
NIST_psdtev<-sqrt(NIST_variance)


#calculate residuals
df$sensor_res<-(df$SensorAvg-u_sensor)

#remove 0 values
df_lag<-df %>% 
  mutate(sensor_res_lag= if_else(is.na(lag(sensor_res))==F,lag(sensor_res),  -999)) %>% 
  filter(sensor_res_lag>-998)
  
           
#Plot scatter for correlation coefficient 
         
require(ggplot2)               # To derive the graphs
require(ggthemes)              # To apply ggplot themes to the chart
require(scales)                # For pretty breaks

# Function to generate correlation coefficient for the charts
corr_eqn <- function(x,y, digits = 4) {
  corr_coef <- round(cor(x, y), digits = digits)
  paste("italic(r) == ", corr_coef)
}

# Provide a scatter plot 
plot<-ggplot(df_lag, aes(x = sensor_res_lag, y = sensor_res)) +
  geom_point(shape = 19, size = 2, na.rm=TRUE) +
  geom_smooth(colour = "red", fill = "lightgreen", method = 'lm', na.rm=TRUE) +
  ggtitle("EastBuxton 2016-05 RH") +
  xlab("sensor_res") +
  ylab("sensor_res_lag") +
  scale_colour_tableau(
    palette = "Tableau 10",
    type = "regular",
    direction = 1) +
  geom_text(data = labels, aes(x = x, y = y,
                               label = label), parse = TRUE) +
  theme(legend.key = element_blank(),
        legend.background = element_rect(colour = 'black'),
        legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(lineheight = .8, face = "bold", vjust = 1),
        axis.text.x = element_text(size = 11, vjust = 0.5,
                                   hjust = 1, colour = 'black'),
        axis.text.y = element_text(size = 11, colour = 'black'),
        axis.title = element_text(size = 10, face = 'bold'),
        axis.line = element_line(colour = "black"),
        plot.background = element_rect(colour = 'black', size = 1),
        panel.background = element_blank())
  #annotate(x=1.5, y=1.5, 
           #label=paste("R = ", round(cor(df_lag$sensor_res, df_lag$sensor_res_lag),2)), 
           #geom="text", size=5)
labels = data.frame(x = 2, y = 2, label = corr_eqn(df_lag$sensor_res, df_lag$sensor_res_lag))

TTEST2<-t.test(df$SensorAvg,df$NistAvg, var.equal = TRUE)


plot(plot)


