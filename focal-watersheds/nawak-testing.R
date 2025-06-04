depth18<-plot_ly(depth_2019, x = ~Date, y = ~DepthTUNA1PT_Avg, 
        type = 'scatter', 
        mode = 'lines',
        text = ~DepthTUNA1PT_Q_flags,
        hovertemplate = paste('%{x}',
                        '%{text}', 
                        '%{y}',
                        '<extra></extra>'))%>%
  layout(title = " ", #barmode = 'stack',
         xaxis = list(title = "Date"),
         yaxis = list(side="left", title = 'Water Level [m]'))

depth18






saveWidget(temp18, file = "temp18.html")
saveWidget(temp19, file = "temp19.html")
saveWidget(depth18, file = "depth18.html")
saveWidget(depth19, file = "depth19.html")

depth18<-plot_ly(depth_2019, x = ~Date, y = ~DepthTUNA1PT_Avg, 
                 type = 'scatter', 
                 mode = 'lines',
                 text = ~DepthTUNA1PT_Q_flags,
                 hovertemplate = paste('%{x}',
                                       '%{text}', 
                                       '%{y}',
                                       '<extra></extra>'))%>%
  layout(title = " ", #barmode = 'stack',
         xaxis = list(title = "Date"),
         yaxis = list(side="left", title = 'Water Level [m]'))

depth18

```
```{r 2018_temp, include=FALSE}
temp18<-plot_ly(temp, x = ~Date, y = ~TWtrTUNA1PT_Avg, 
                type = 'scatter', mode = 'lines', name = 'Twtr_PT', text = ~TWtrTUNA1PT_Q_flags,
                hovertemplate = paste('%{x}',
                                      '%{text}', 
                                      '%{y}',
                                      '<extra></extra>')) %>%
  layout(title = " ", #barmode = 'stack'
         name = "Twtr_PT",
         xaxis = list(title = "Date"),
         yaxis = list(side="left", title = 'Water Temperature [degC]'))


# Add the first trace to the existing plot
temp18 <- temp18 %>% add_trace(
  data = temp,
  x = ~Date,
  y = ~TWtrTUNA1_TB1_Avg,
  type = "scatter",
  mode = "lines",
  name = "Tidbit"
)

temp18
```

##### 2019
```{r depth-2019, include=TRUE}

depth19<-plot_ly(depth_2019, x = ~Date, y = ~DepthTUNA1PT_Avg, 
                 type = 'scatter', 
                 mode = 'lines',
                 text = ~DepthTUNA1PT_Q_flags,
                 hovertemplate = paste('%{x}',
                                       '%{text}', 
                                       '%{y}',
                                       '<extra></extra>'))%>%
  layout(title = " ", #barmode = 'stack',
         xaxis = list(title = "Date"),
         yaxis = list(side="left", title = 'Water Level [m]'))

depth19


```

```{r temp-2019, include=TRUE}
temp19<-plot_ly(temp, x = ~Date, y = ~TWtrTUNA1PT_Avg, 
                type = 'scatter', mode = 'lines', name = 'Twtr_PT', text = ~TWtrTUNA1PT_Q_flags,
                hovertemplate = paste('%{x}',
                                      '%{text}', 
                                      '%{y}',
                                      '<extra></extra>')) %>%
  layout(title = " ", #barmode = 'stack'
         name = "Twtr_PT",
         xaxis = list(title = "Date"),
         yaxis = list(side="left", title = 'Water Temperature [degC]'))


# Add the first trace to the existing plot
temp19 <- temp19 %>% add_trace(
  data = temp,
  x = ~Date,
  y = ~TWtrTUNA1_TB1_Avg,
  type = "scatter",
  mode = "lines",
  name = "Tidbit"
)

temp19


####  Annual Data {.tabset}
##### 2018
```{r depth-2018, include=TRUE}

include_graphics("depth18.html")


```

##### 2019
```{r temp-2018, include=TRUE}

```{r graph_prep, include=FALSE}
depth_2018<-depth %>% 
  select(1:4) %>%
  filter(Date >= as_datetime("2018-08-21 23:00:00"), Date <= as_datetime("2018-12-31 23:55:00"))
temp_2018<-temp %>% 
  select(1:7) %>% 
  filter(Date >= as_datetime("2018-08-21 23:00:00"), Date <= as_datetime("2018-12-31 23:55:00"))

depth_2019<-depth %>% 
  select(1:4) %>%
  filter(Date >= as_datetime("2019-01-01 00:00:00"), Date <= as_datetime("2019-12-31 23:55:00"))
temp_2019<-temp %>% 
  select(1:7) %>% 
  filter(Date >= as_datetime("2019-01-01 00:00:00"), Date <= as_datetime("2019-12-31 23:55:00"))

depth_2020<-depth %>% 
  select(1:4) %>%
  filter(Date >= as_datetime("2020-01-01 00:00:00"), Date <= as_datetime("2020-12-31 23:55:00"))
temp_2020<-temp %>% 
  select(1:7) %>% 
  filter(Date >= as_datetime("2020-01-01 00:00:00"), Date <= as_datetime("2020-12-31 23:55:00"))

depth_2021<-depth %>% 
  select(1:4) %>%
  filter(Date >= as_datetime("2021-01-01 00:00:00"), Date <= as_datetime("2021-12-31 23:55:00"))
temp_2021<-temp %>% 
  select(1:7) %>% 
  filter(Date >= as_datetime("2021-01-01 00:00:00"), Date <= as_datetime("2021-12-31 23:55:00"))

depth_2022<-depth %>% 
  select(1:4) %>%
  filter(Date >= as_datetime("2022-01-01 00:00:00"), Date <= as_datetime("2022-12-31 23:55:00"))
temp_2022<-temp %>% 
  select(1:7) %>% 
  filter(Date >= as_datetime("2022-01-01 00:00:00"), Date <= as_datetime("2022-12-31 23:55:00"))

depth_2023<-depth %>% 
  select(1:4) %>%
  filter(Date >= as_datetime("2023-01-01 00:00:00"), Date <= as_datetime("2023-12-31 23:55:00"))
temp_2023<-temp %>% 
  select(1:7) %>% 
  filter(Date >= as_datetime("2023-01-01 00:00:00"), Date <= as_datetime("2023-12-31 23:55:00"))


```
include_graphics("temp18.html")

```

```{r temp-2019, include=TRUE}

include_graphics("temp19.html")

```
#Sowder, C. and E.A. Steel. 2012. A note on the collection and cleaning of water temperature data. Water (4): 597-606.

#data_flagged<-df %>% 
#mutate(temp_diff = lead(TWtrTUNA1PT_Avg)-lag(TWtrTUNA1PT_Avg),
#depth_diff= lead(DepthTUNA1PT_Avg)-lag(DepthTUNA1PT_Avg),
#qc_flag = case_when(is.na(DepthTUNA1PT_Avg) ~ "MV: QC'd by EH"
#  ,TWtrTUNA1PT_Avg >= 25 ~ "SVC: Max temp: QC'd by EH"
# ,TWtrTUNAPT_Avg < 0 ~ "SVC: Min temp: QC'd by EH"
# ,temp_diff > 1 ~ "SVC: Rate of change exceedance Temperature: QC'd by EH"
#  ,depth_diff > 1 ~ "SVC: Rate of change exceedance: QC'd by EH"
#  ,DepthTUNA1PT_Avg < 0.03 ~  "SVC: Dewatering potential: QC'd by EH"
# ,TRUE ~ "AV: QC'd by EH"))
# Define the start and end dates for the date range

# Define the start and end dates for the date range
start_record <- as.POSIXct("2018-08-21 00:00:00")
end_record <- as.POSIXct("2019-02-06 13:15:00")

# Subtract 3 from the 'value' column if the 'date' falls within the specified range
depth<-depth %>%
  mutate(value = ifelse(Date >= start_record & Date <= end_record, DepthTUNA1PT_Avg - 0.03, DepthTUNA1PT_Avg)) %>% 
  select(-DepthTUNA1PT_Avg) %>% 
  rename(DepthTUNA1PT_Avg=value)


df_filtered <- df %>%
  filter_all(all_vars(!grepl("PLS", .)))
