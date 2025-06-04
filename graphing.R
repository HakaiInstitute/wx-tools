#########################################################################################################################
#Graphing tools
#########################################################################################################################
#load packages
lapply(c("tidyverse", "lubridate", "reshape", "stringr", "plotly", "roll", "data.table", "cdata"), library, character.only = TRUE)

#QC Status
##What has and hasn’t been QC’d? (QC level) -- usually most useful in long format
QC_check<-df %>% 
  ggplot(aes(x=Date, y=value)) + 
  geom_point(aes(shape=Quality_Level, color=QC_Flag)) + 
  theme_linedraw()

#View by site/survey -- does each show expected results?
Site_check<-df %>% 
  ggplot(aes(x=Date, y=value)) +
  theme_linedraw() + 
  geom_point(aes(color=QC_Flag)) + 
  facet_grid(Site~.)

#Can we see expected relationships between variables?
Variable_check<-df %>% 
  ggplot(aes(x=Date, y=temp)) + 
  geom_point(aes(color=Site)) + 
  theme_linedraw()
plot(varia)

temp4<-df %>% 
  ggplot(aes(x=Date,y=temp, group=Site)) + 
  geom_line(aes(col=Site), na.rm=TRUE) + 
  #scale_x_date(date_labels = "%Y-%m-%d")+
  theme_bw()+
  xlab("Date") + ylab("Water Temperature (degC)")
plot(temp4)
str(df)

test<-df_long %>% 
  ggplot(aes(x=Date, y=`Water Temperature`)) + geom_line() +scale_x_date(date_labels = "%Y-%m-%d")
test

df_long$Date<-as.Date(df_long$Date)



ggplot(df_long, aes(x = Date, y = `Water Temperature`, colour = year)) +
    geom_line()

plot(temp2)
##################################################################################################################
#Plotly
##################################################################################################################
##Multiple series time-series data and facet wrap by watyr 
plot<-plot_ly(
  data = df,
  x = ~Date,
  y = ~temp,
  color = ~Site,
  colors="Set1",
  type = "scatter",
  mode = "lines") %>% 
  #name = "Depth"
  #fill = "tozeroy"
  layout(title = " ", #barmode = 'stack',
        xaxis = list(title = "Date", 
                      showgrid=FALSE),
        tickformat = "%d %B (%a)<br>%Y"),
yaxis = list(side="left", 
                      title = 'Water Temperature (degC)', 
                      showgrid=FALSE,
                      showline=TRUE)
plot



layout(title = "horn bar Charts with label",
       titlefont =list(color='red',size=12),
       titleside ='bottom',
       xaxis=list(title='',
                  showgrid=F,
                  side='bottom',
                  #nticks=10,
                  #ticks='inside',
                  ticksuffix='???',
                  tickfont=list(color='blue')),
       yaxis=list(title='',
                  showgrid=FALSE,
                  #type="category" ,
                  #ticks='inside',
                  #color='red',
                  #linecolor=toRGB('red'),
                  zeroline=FALSE,
                  #zerolinecolor = toRGB("red"),
                  showline=FALSE,
                  showticklabels=FALSE
                  #mirror='all',
       ))

plot<-plot %>% add_trace(
  data = data,
  x = ~Date,
  y = ~TWtrLULL1PT_Avg,
  #color = ~name,
  type = "scatter",
  mode = "lines",
  name = "PT2_temp",
  data = new_df,
  x = ~Date,
  y = ~SCKoeye1_Avg,
  #color = ~name,
  type = "scatter",
  mode = "lines",
  name = "EC_Temp",

  fill = "tozeroy"
)

plot<-plot %>% add_trace(
  data = data,
  x = ~Date,
  y = ~TWtrLULL1_TB1_Avg,
  #color = ~name,
  type = "scatter",
  mode = "lines",

  data = new_df,
  x = ~Date,
  y = ~TWtrKoeyePT2_Avg,
  #color = ~name,
  type = "scatter",
  mode = "lines",
  name = "PT2_temp",
  fill = "tozeroy"
)

plot<-plot %>% add_trace(
  data = new_df,
  x = ~Date,
  y = ~TWtrKoeyePT_Avg,
  #color = ~name,
  type = "scatter",
  mode = "lines",
  name = "PT_temp",
  fill = "tozeroy"
)

fig <- plot %>% layout(xaxis = list(title = 'Date'),
                      yaxis = list(title = 'Water Temperature (degC)'))
fig <- fig + facet_wrap( ~ watyr, ncol=2)

fig <- ggplotly(fig)

fig


#################################################################################################################
#Multiple series overlay -- plotly
#################################################################################################################
plot2<-plot_ly(data_twtr, x = ~Date, y = ~TWtrLULL1PT_Avg, 
        type = 'scatter', mode = 'lines', name = 'Lull Creek') %>%
  add_trace(x = ~Date, y = ~TWtrGLEN1PT_Avg, 
            type = 'scatter', mode = 'lines', name = 'Glendale River', 
            yaxis = 'y') %>%
  add_trace(x = ~Date, y = ~TWtrFULL1PT_Avg, 
            type = 'scatter', mode = 'lines', name = 'Fulmore River', 
            yaxis = 'y') %>%
  add_trace(x = ~Date, y = ~TWtrTUNA1PT_Avg, 
            type = 'scatter', mode = 'lines', name = 'Tuna River', 
            yaxis = 'y') %>%
  add_trace(x = ~Date, y = ~TWtrHEYD2PT_Avg, 
            type = 'scatter', mode = 'lines', name = 'Heydon Creek', 
            yaxis = 'y') %>%
    layout(title = " ", #barmode = 'stack',
         xaxis = list(title = "Date"),
         yaxis = list(side="left", title = 'Water Temperature (degC)'))
        # yaxis2 = list(side = 'right', overlaying = "y", title = 'Water Temperature (degC)',
                       #showgrid = FALSE, zeroline = FALSE)
####################################################################################################################
#ggplotly interface
####################################################################################################################
ggplotly(ggplot() + 
           geom_point(data = tip_analysis, aes(x = time_pst, y = Hakai_level, Label = date), shape = 21, size = 2) + 
           theme_bw() + xlab("Hakai Stage [m]") + ylab("Atlas Stage [m]") + labs(fill = 'Storm event no.'))

plot<-ggplotly(ggplot() + 
           geom_line(data = data, aes(x = Date, y = DepthLULL1PT_Avg, color = Download_Period)) +
             geom_hline(yintercept=0.18)+
                        theme_bw() + xlab("Date") + ylab("Water Level [m]") + labs(fill = 'Date'))


####################################################################################################################
#ggplot2
####################################################################################################################
#specify text theme to apply to all figures
My_Theme = theme(
  axis.title.x = element_text(size = 12),
  axis.text.x = element_text(size = 10),
  axis.title.y = element_text(size = 12))

#add multiple series to one figure
temp<-temp_data %>% 
  ggplot() + 
  geom_line(data = temp_data, aes(x = Date, y = TWtrKoeye1_Avg), color = "red") +
  geom_line(data = temp_data, aes(x = Date, y = TWtrKoeyePT_Avg), color = "blue") +
  geom_line(data = temp_data, aes(x = Date, y = TWtrKoeyePT2_Avg), color = "green")+
  xlab('Date') +
  ylab('Temperature (degC')

plot(temp)

temp<-df_long %>% 
  ggplot() + 
  geom_line(data = df_long, aes(x = Date, y = Water_Temperautre), group = Year)+ 
  theme_bw()+
  scale_x_date("Date", date_breaks = "6 months") + 
  xlab('Date') +
  ylab('Temperature (degC)')

plot<-(ggplot() + 
         geom_line(data = df, aes(x = Date, y = TWtrKoeyePT1_Avg))+
         geom_line(data = df, aes(x = Date, y = TWtrKoeyePT2_Avg))+
         geom_line(data = df, aes(x = Date, y = PLS_TempAvg))+
         theme_bw() + 
         scale_x_date("Date", date_breaks = "1 year", date_labels = "%Y")+
         xlab("Date") + 
         ylab("Water Temperature [�C]"))
plot


ggsave('test.jpg', plot, device = "jpg", dpi = 500)

 summary(df_long)
head(df_long)
plot(temp)


#wrap by site
temp_wrap<-temp_long %>% 
  ggplot(aes(x=Date, y=value)) +
  theme_linedraw() + 
  geom_line() + 
  ylab('Temperature (degC)')+
   facet_grid(variable~.)

g<-temp_wrap+My_Theme

plot(g)
####################################################################################################################
#Histogram/Frequency Table
####################################################################################################################
#calculate bin width
breaks <- pretty(range(df$num_tips), n = nclass.FD(df$num_tips), min.n = 1)
bwidth <- breaks[2]-breaks[1]
dft <- data.frame(df$num_tips)

ggplot(df, aes(df))+geom_histogram(binwidth=num_bins,fill="white",colour="black")


num_bins <- diff((range(df$num_tips)) / (2 * IQR(df$num_tips) / length(df$num_tips)^(1/3)))

histogram<-ggplot(df, aes(x = num_tips)) +
  geom_histogram() +  facet_grid(vars(sensor_node))


histogram

#frequency table
freq <- data.frame(table(df))

relFreq <- data.frame(prop.table(table(df)))
relFreq$Relative_Freq <- relFreq$Freq
relFreq$Freq <- NULL

Cumulative_Freq <- cumsum(table(df))

z <- cbind(merge(freq, relFreq), Cumulative_Freq)
z$Cumulative_Relative_Freq <- z$Cumulative_Freq / sum(z$Freq)

print(z)


freq_table<-df %>% 
  mutate(df, relFreq = prop.table(df), Cumulative_Freq = cumsum(df), 
         Cumulative_Relative_Freq = cumsum(relFreq))

#plot histogram
hist<-ggplot(df, aes(x=Rain))+
  geom_histogram(fill="black", position="dodge", bins=30)+
  theme_bw()+
  theme(legend.position = "top")+
  labs(x="1 Hr Rain", y="Frequency")+theme_bw()
plot(hist)

#######################################################################################################################
#Correlation matrix plots
#######################################################################################################################
##data must be in long format

#rename columns for space-saving on facet grid labels
library(plyr)
df<-rename(rain_spread, c("Rain24HrPruthMtd"="Pruth", "Rain24HrSSN1015Mtd"="SSN1015","Rain24HrSSN626Mtd"="SSN626","Rain24HrSSN693Mtd"="SSN693","Rain24HrSSN708Mtd"="SSN708","Rain24HrSSN819Mtd"="SSN819", "Rain24HrTSN3Mtd"="TSN3","Rain24HrWSN703Mtd"="WSN703","Rain24HrWSN693_703Mtd"="WSN693703","Rain24HrWSN703_708Mtd"="WSN703708"))

#Separate by WY
WY14_wide<-df %>% 
  filter(Date >= as.Date("2013-10-01"), Date <= as.Date("2014-09-01"))
WY15_wide<-df %>% 
  filter(Date >= as.Date("2014-10-01"), Date <= as.Date("2015-09-01"))
WY16_wide<-df %>% 
  filter(Date >= as.Date("2015-10-01"), Date <= as.Date("2016-09-01"))
WY17_wide<-df %>% 
  filter(Date >= as.Date("2016-10-01"), Date <= as.Date("2017-09-01"))
WY18_wide<-df %>% 
  filter(Date >= as.Date("2017-10-01"), Date <= as.Date("2018-09-01"))
WY19_wide<-df %>% 
  filter(Date >= as.Date("2018-10-01"), Date <= as.Date("2019-09-01"))

#remove unneeded columns for SPLOMs
df14<-WY14_wide[-c(1:4)]
df15<-WY15_wide[-c(1:4)]
df16<-WY16_wide[-c(1:4)]
df17<-WY17_wide[-c(1:4)]
df18<-WY18_wide[-c(1:4)]
df19<-WY19_wide[-c(1:4)]


library(cdata)
#specify variables to plot
meas_vars<-colnames(df_cor)#[2:6]

#data.frame() call strips attributed from the frame returned by expand.grid()
controlTable<-data.frame(expand.grid(meas_vars, meas_vars,
                                     stringsAsFactors = 
                                       FALSE))
#rename columns
colnames(controlTable)<-c("x","y")

#add the key column
controlTable<-cbind(data.frame(pair_key=paste(controlTable[[1]], controlTable[[2]]),
                               stringsAsFactors = FALSE),
                    controlTable)
#change df name
df_me_aug = rowrecs_to_blocks(
  df_cor #remember to change per df
  ,
  controlTable)

splt <- strsplit(df_me_aug$pair_key, split = " ", fixed = TRUE)

df_me_aug$xv <- vapply(splt, function(si) si[[1]], character(1))

df_me_aug$yv <- vapply(splt, function(si) si[[2]], character(1))

head(df_me_aug)

df_me_aug$xv <- factor(as.character(df_me_aug$xv),
                       meas_vars)
df_me_aug$yv <- factor(as.character(df_me_aug$yv),
                       meas_vars)

#plot
SPLOM_wsc<-ggplot(df_me_aug, aes(x=x, y=y)) +
  geom_abline(slope=1, intercept=0)+
  geom_point() + theme_bw(base_size=12)+ guides(color=guide_legend(title=" "))+
  xlim(0,6.1)+
  ylim(0,6.1)+
  facet_grid(yv~xv, scale="free")+ #labeller = label_both
  ggtitle("WY15 Precipitation") +
  ylab(NULL) + 
  xlab(NULL)+
  #scale_color_brewer(palette = "Dark2") +
  theme(strip.text.x = element_text(size = 8),
        strip.text.y = element_text(size = 8))

plot(SPLOM_wsc)

######################################################################################################################
#Windrose plots
######################################################################################################################
#plotly high-contrast bw plots
data<-df %>%
  mutate(
    Month = as.character(Month),
    Season = case_when(
      Month == "Oct"|Month == "Nov"|Month == "Dec"|Month == "Jan"|Month == "Feb"|Month == "Mar"|Month == "Apr" ~ "Wet",
      Month == "May"|Month == "Jun"|Month == "Jul"|Month == "Aug"| Month =="Sep" ~ "Dry",
      TRUE ~ Month))

dry<-data %>% 
  select(Date:Season) %>% 
  filter(Season=="Dry")

wet<-data %>% 
  select(DateTime:Season) %>% 
  filter(Season=="Wet")

# set # of hours to reflect n per season -- run code for each "season"
plotWindRose <- function(data, hrs = 35481, plotTitle = "", dirres = 15)
  
  #set breaks for direction  
  dirres <- 15
dir.breaks <- c(-dirres/2,
                seq(dirres/2, 360-dirres/2, by = dirres),
                360+dirres/2)
dir.labels <- c(seq(0, 360, by = dirres))

#wrangle data for plot
wind <- df %>%
  filter(Date >= startTime) %>%
  select(Date, WindSpdQuadraMax, WindDirQuadraAvg) %>%
  mutate(ws_bin = cut(WindSpdQuadraMax, breaks=c(-Inf, 5, 10, 20, 30, 40, 50, Inf), labels = c("< 5 km/h", "5-10 km/h", "10-20 km/h", "20-30 hm/h", "30-40 km/h", "40-50 km/h",">50 km/h"))) %>%
  mutate(wd_bin = cut(WindDirQuadraAvg, breaks = dir.breaks, labels = dir.labels)) %>%
  group_by(wd_bin, ws_bin) %>%
  summarise(Freq=(n()/24)*100) %>%
  arrange(ws_bin)

#plot
plot <- plot_ly(wind, type = "barpolar", hovertemplate = paste('Freq (%): %{r:.2f}<br>Dir: %{theta}\u00b0;'), colors = c("#CCCCCC", "#666666","#000000","#EEEEEE")) %>%
  add_trace(r = ~Freq, theta = ~wd_bin, color = ~ws_bin) %>%
  layout(
    plot_bgcolor = "#FFFFFF",
    paper_bgcolor = "#FFFFFF",
    autosize = TRUE,
    margin = list(l = 10, r = 10),
    annotations = list(text = " ", xanchor = "centre",
                       yref="paper",y=1,yshift=50,showarrow=FALSE,
                       font=list(size=18,color='rgb(0,0,0)')),
    xaxis = list(
      title = c("N", "NE", "E", "SE", "S", "SW", "W", "NW")
    ),
    legend = list(orientation = "h"),
    polar = list(
      radialaxis = list(
        nticks = 7,
        angle = 45,
        tickangle = 45,
        ticksuffix = " %"
      ),
      angularaxis = list(
        tickmode = "array",
        tickvals = c(0 , 45, 90, 135, 180, 225, 270, 315),
        ticktext = c("N", "NE", "E", "SE", "S", "SW", "W", "NW"),
        direction = "clockwise"
      )
    )
  )
plot

#clifro colour plots
library(clifro)

data<-df %>%
  mutate(
    Month = as.character(Month),
    Season = case_when(
      Month == "Oct"|Month == "Nov"|Month == "Dec"|Month == "Jan"|Month == "Feb"|Month == "Mar"|Month == "Apr" ~ "Wet",
      Month == "May"|Month == "Jun"|Month == "Jul"|Month == "Aug"| Month =="Sep" ~ "Dry",
      TRUE ~ Month))
windrose(WindSpdQuadraMax, WindDirQuadraAvg,n_col=2,
                                   speed_cuts = c(0,5,10, 15, 20,25,30),
                         

seasonal_wind<-with(data,           legend_title = "Wind Speed\n(m/s)",
                                   legend.title.align = .5,
                                   ggtheme = "linedraw",
                                   col_pal = "Greys"))
seasonal_wind + theme(
  panel.grid.major = element_line(colour = "black"))

seasonal_wind + scale_colour_identity()

plot(seasonal_wind)

##VIU Raw plot
# creates a plotly wind rose using data from viu cleaned df tables time is number of hours
plotWindRose <- function(dry, hrs = 43, plotTitle = "", dirres = 15){
  
  if(all(dry$Wind_Speed == 0) | length(dry$Wind_Speed) < (hrs / 2)){ # account for stations that dont have ws sensors and if there have been gaps for over half the data period
    plot <- plotly_empty(type = "barpolar") %>%
      layout(
        margin = list(t = 60, l = 20, r = 20),
        annotations = list(text = " ", xanchor = "centre",
                           yref="paper",y=1,yshift=50,showarrow=FALSE,
                           font=list(size=18,color='rgb(0,0,0)')),
        xaxis = list(
          title = c("N", "NE", "E", "SE", "S", "SW", "W", "NW")
        ),
        legend = list(orientation = "h"),
        polar = list(
          radialaxis = list(
            nticks = 7,
            angle = 45,
            tickangle = 45,
            ticksuffix = " %"
          ),
          angularaxis = list(
            tickmode = "array",
            tickvals = c(0 , 45, 90, 135, 180, 225, 270, 315),
            ticktext = c("N", "NE", "E", "SE", "S", "SW", "W", "NW"),
            direction = "clockwise"
          )
        )
      )
  } else {
    dirres <- 15
    dir.breaks <- c(-dirres/2,
                    seq(dirres/2, 360-dirres/2, by = dirres),
                    360+dirres/2)
    dir.labels <- c(seq(0, 360, by = dirres))
    
    startTime <- (Sys.time() - hours(8))-hours(hrs)
    
    wind <- df %>%
      #filter(DateTime >= startTime) %>%
      select(Date, WindSpdLookoutMax, WindDirLookoutAvg) %>%
      mutate(ws_bin = cut(WindSpdLookoutMax, breaks=c(-Inf, 5, 10, 20, 30, 40, 50, Inf), labels = c("< 5 km/h", "5-10 km/h", "10-20 km/h", "20-30 km/h", "30-40 km/h", "40-50 km/h",">50 km/h"))) %>%
      mutate(wd_bin = cut(WindDirLookoutAvg, breaks = dir.breaks, labels = dir.labels)) %>%
      group_by(wd_bin, ws_bin) %>%
      summarise(Freq=(n()/hrs)*100) %>%
      arrange(ws_bin)
    
    dry <- plot_ly(wind, type = "barpolar", hovertemplate = paste('Freq (%): %{r:.2f}<br>Dir: %{theta}\u00b0;'), colors = c("#4575B4", "#91BFDB", "#E0F3F8", "#FEE090", "#FC8D59", "#D73027")) %>%
      add_trace(r = ~Freq, theta = ~wd_bin, color = ~ws_bin) %>%
      layout(
        plot_bgcolor = "#f5f5f5",
        paper_bgcolor = "#f5f5f5",
        autosize = TRUE,
        margin = list(l = 10, r = 10),
        annotations = list(text = " ", xanchor = "centre",
                           yref="paper",y=1,yshift=50,showarrow=FALSE,
                           font=list(size=18,color='rgb(0,0,0)')),
        xaxis = list(
          title = c("N", "NE", "E", "SE", "S", "SW", "W", "NW")
        ),
        legend = list(orientation = "h"),
        polar = list(
          radialaxis = list(
            nticks = 7,
            angle = 45,
            tickangle = 45,
            ticksuffix = " %"
          ),
          angularaxis = list(
            tickmode = "array",
            tickvals = c(0 , 45, 90, 135, 180, 225, 270, 315),
            ticktext = c("N", "NE", "E", "SE", "S", "SW", "W", "NW"),
            direction = "clockwise"
          )
        )
      )
  }
  dry
}
plotWindRose(dry, plotTitle = "Dry", dirres = 15)
############################################################################
splom <- ggpairs(
  df_cor,
  upper = list(continuous = wrap("density", alpha = 0.5), combo = "box"),
  lower = list(continuous = wrap("points", alpha = 0.3,    size=0.1), 
               combo = wrap("dot", alpha = 0.4,            size=0.2) ),
  title = "Diamonds"
)
splom
