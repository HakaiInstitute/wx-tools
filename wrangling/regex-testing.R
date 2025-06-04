library("tidyverse")
tib <- tibble(x = c("lemon", "yellow, banana", "red, big, apple"))

test1<-people %>% 
  separate(Name, into =c("FIRST","LAST"), sep=" "))
#c column vector
#sep = what thing you want to separate by -- default is whitespace


#each row consists of 1 observation

#pivot_longer(dataframe, which columns contain values and variables, names_to="Observation", values_to="values")
#pivot_longer(dataframe, columns, names_to = c("Day", "Time"), names_sep="-")

df_sep<-test_df %>% 
  pivot_longer(Qlevel_RHBuxtonEast:RHWSN693703_Avg, names_to = c("Qlevel","Qflag", "Value"), names_sep="_") %>% 
  view()


df_test<-df %>% 
  separate(df, into = c("Qlevel","Qflag", "Value"), '('Qflag|Qlevel)?_?(RH[^_])+))

niceSiteNames = gsub("_", " ", station_id),

#separate column regex
df_regex <- df %>% 
  separate(col=condition, into= c('Qlevel'), sep='_')

df.rh.2 <- tmp2 %>%
  mutate(Qlevel= ifelse(grepl("Qlevel", site), rh.perc,-99999),
         Qflag= ifelse(grepl("Qflag", site),rh.perc,-99999),
         rh.perc= ifelse(grepl("Avg", site), rh.perc, -99999))

df_regex_short<-tmp2 %>% 
  filter(Qflag>-99999,
         Qlevel>-99999,
  )


#wide to long
df_wide<-dcast(df_regex, condition ~ measurement, value.var="value")

df_wide <- reshape(df_regex, idvar = "condition", 
                   timevar = "measurement", direction = "wide")

Data_trend <-
  data %>%
  tidyr::pivot_longer(
    cols = !`Trade Value`,
    names_to = c("Position", "Player Name"),
    names_pattern = c("(.*).(.*)"),
  )

data %>%
  pivot_longer(-1, 
               names_to = c("Position", ".value"), 
               names_pattern = c("([^\\.]+)\\.([^\\.]+)"))

df_regex<-test_df %>%
  pivot_longer(cols= Qlevel_RHBuxtonEast:RHWSN693703_Avg, names_to = c("Position", ".value"), 
               names_pattern = c("(Qflag|Qlevel)?_?(RH[^_]+)"))

#(Qflag|Qlevel)?_?(RH[^_])+)




























