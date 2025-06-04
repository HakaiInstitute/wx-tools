head(orange)
head(airquality)
?orange
?Orange
head(Orange)


#pivot wider
orange2<-Orange %>% 
  pivot_wider(names_from = "age", 
              values_from="circumference")

#pivot_longer
orange3<-orange2 %>% 
  pivot_longer(cols=2:8,
               names_to="Age",
               values_to="CIRC")
df1<- df %>% 
  select(-contains("UNESCO"))

#change column positions
df1<-c(1,2,3,4,8,15,21,5,6,12,13,19,20,7,9,10,11,14,16,17,18)
df2<-df1[ , c(1,2,3,4,8,15,21,5,6,12,13,19,20,7,9,10,11,14,16,17,18)]




df_longavg <-df_avg %>% 
  pivot_longer(cols=!Date,
               names_to="Sensor",
               values_to="Value")

df_level <-dfl %>% 
  pivot_longer(cols=!Date,
               names_to="Sensor",
               values_to="q_level")


df_flag<-dff %>% 
  pivot_longer(cols=!Date,
               names_to="Sensor",
               values_to="q_flag")




df_long1<-df_long %>% 
  pivot_longer(cols = 5:7,
               names_to = ('level'),
               names_pattern = "(...?_?L)")

#create ID column for merge
df_flag<-rowid_to_column(df_flag, "ID")
df_level<-rowid_to_column(df_level, "ID")
df_longavg<-rowid_to_column(df_longavg, "ID")

#left join
df_lj <- left_join(df_level, df_flag, by=c("ID","Date"))
df_koeye <- left_join(df_lj, df_longavg, by=c("ID","Date"))

write.csv(df_koeye,"koeye-time-series-2023.csv")

df = select(df_koeye, -c(1,3,5))

df_long<-d1f %>% 
  pivot_longer(cols=!Date,
               names_to = c('sensor','flag'),
               names_pattern = '(...)?_?(.)',
               values_to = 'value')
               
df_long <- df %>%
  pivot_longer(
    cols = -c(Date, Year, Month, WaterYear),
    names_to = c("sensor", "flag"),
    names_pattern = "^(\\w+)_?(\\w*)",
    values_to = "value"
  )


#create ID column for merge
df_flag<-rowid_to_column(df_flag, "ID")
df_level<-rowid_to_column(df_level, "ID")
df_longavg<-rowid_to_column(df_longavg, "ID")

#left join
df_lj <- left_join(df_level, df_flag, by=c("ID","Date"))
df_koeye <- left_join(df_lj, df_longavg, by=c("ID","Date"))

df_long_numeric <- df %>%
  select(-starts_with("Q_flags")) %>%
  pivot_longer(
    cols = -c(Date, Year, Month, WaterYear),
    names_to = "sensor",
    values_to = "value"
  )

df_long_character <- df %>%
  select(Date, Year, Month, WaterYear, starts_with("Q_flags")) %>%
  pivot_longer(
    cols = -c(Date, Year, Month, WaterYear),
    names_to = "flag",
    values_to = "value"
  )


  #d1f<-df_regex %>% 
  #select(-contains("level"))


data_SVC <- df(grepl(df, q_flag == 'SVC')
               
               data_SVC<- df %>% 
                 filter(grepl('SVC', q_flag))
               
            `df %>% filter(grepl('Dev', roles))

               # Separate columns with mixed types
               your_data_frame <- df %>%
                 separate(
                   col = DepthKoeyePT2_Q_flags,
                   into = c("DepthKoeyePT2_Q_level", "DepthKoeyePT2_Q_flags"),
                   sep = ": ",
                   remove = FALSE
                 ) %>%
                 separate(
                   col = TWtrKoeyePT2_Q_flags,
                   into = c("TWtrKoeyePT2_Q_level", "TWtrKoeyePT2_Q_flags"),
                   sep = ": ",
                   remove = FALSE
                 )
               
               # Now, you can pivot the numeric columns
               df_long_numeric <- your_data_frame %>%
                 pivot_longer(
                   cols = c(DepthKoeyePT2_Q_level, TWtrKoeyePT2_Q_level, DepthKoeyePT2_Avg, TWtrKoeyePT2_Avg),
                   names_to = "sensor",
                   values_to = "value"
                 )
               
               In the code above:
                 
                 We use the separate function to split the mixed columns (DepthKoeyePT2_Q_flags and TWtrKoeyePT2_Q_flags) into two separate columns, one for the level (*_Q_level) and one for the flags (*_Q_flags). We use sep = ": " to split the character values based on the colon and space.
               
               After separating the columns, we can pivot the numeric columns with pivot_longer.
               
               This approach handles the mixed types in your data, allowing you to pivot the numeric values while preserving the character values.
               
               your_data_frame <- your_data_frame %>%
                 pivot_longer(
                   cols = starts_with("DepthKoeyePT2"),
                   names_to = c("sensor", ".value"),
                   names_sep = "_"
                 ) %>%
                 pivot_longer(
                   cols = starts_with("TWtrKoeyePT2"),
                   names_to = c("sensor", ".value"),
                   names_sep = "_"
                 )
               