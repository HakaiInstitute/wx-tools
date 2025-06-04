# Temperature logger data

# Clear workspace
rm(list = ls())

# Install packages (remove comment on below line if you don't have these packages yet)
install.packages(c("tidyverse", "lubridate", "magrittr", "maptools"))

# load packages
lapply(c("tidyverse", "lubridate", "magrittr", "maptools"), library, character.only = TRUE)

# Set working directory directly in console to your file locations. The rest of
#   this script uses relative paths, so this is an essential step for the code
#   to work.
# setwd("/YOUR/FILE/LOCATIONS")

# check your working directory is in the right spot
getwd()

#---- LOAD DATA ----
# Load temperature data #
## Read headers in first
temperatureHeaders <- read.csv("2022-04-27.5minuteSamples.csv",
                               nrows = 1, as.is = TRUE,
                               header = FALSE)

## Read data in, drop redundant header rows
tmp <- read.csv("2022-04-27.5minuteSamples.csv",
                skip = 0,
                header = FALSE,
                stringsAsFactors = FALSE)

## Read in metadata
meta <- read.csv("2022-04-19.1hourSamples.last12weeks.csv",
                 col.names = c("station", "station_id", "nearby_ocean_id", "date_commissioned",
                               "date_decommissioned", "status", "substrate_type", "tide_zone",
                               "depth_m_cd", "latitude_dd", "longitude_dd"))


#---- FORMAT DATA ----
## Add headers to temperature dataframe
colnames(tmp) <- temperatureHeaders
names(tmp)
glimpse(tmp)

tmp2 <- tmp %>% 
  gather(site, temp.degC, - `Measurement time`, - Year, - Month, - WaterYear) %T>%
  glimpse %>%
  select(`Measurement time`,
         Year,
         Month,
         WaterYear,
         site,
         temp.degC)

# Split site and measure from site var; convert temperature to numeric, round to 3 decimal places
temperature <- 
  tmp2 %>% 
  rename(dateTime   = `Measurement time`,
         station_id = site) %>%
  mutate(station_id    = gsub("TAir", "", station_id),#removes TWtr from name
         measure       = substr(station_id, nchar(station_id) - 3 + 1, nchar(station_id)),#creates column for "Avg" etc. 
         station_id    = gsub("_*[[:upper:]]{1}_*.{2,5}$", "", station_id),#parses out Qflag and Qlevel from end of string
         station_id    = tolower(station_id),#converts to lower case
         niceSiteNames = gsub("_", " ", station_id),#replaces any _ with a space
         niceSiteNames = gsub("_sgrass", "", niceSiteNames),#removes instances of sgrass and replaces with a space
         niceSiteNames = gsub(" $", "", niceSiteNames), #removes $ character
         temp.degC     = as.numeric(temp.degC), #sets column structure
         dateTime      = ymd_hms(dateTime)) %>%
  mutate_if(is.numeric, round, 3)

## Fix classes in metadata
meta %<>%
  mutate(station_id = as.character(station_id),
         date_commissioned = ymd(as.character(date_commissioned)),
         date_decommissioned = ymd(as.character(date_decommissioned)))

## Merge station metadata with temperature data
temperature <- 
  left_join(temperature, meta)

# ---- CLEAN DATA ----
# Plot all temperature data
temperature %>% filter(measure == "Med") %>%
  ggplot(aes(x = dateTime, y = temp.degC, col = niceSiteNames)) +
  geom_line() +
  ylab("Temperature (ºC)") +
  xlab("Date") +
  theme_classic() +
  theme(legend.title = element_blank())

# Flag temperature values that appear questionable
## flag schema: AC - acceptable; SVC - suspicious value, caution; SVD - suspicious value, delete
temperature_flagged <-
  temperature %>%
  # subset out only median values to simplify dataset
  filter(measure == "Med") %>% 
  mutate(qc_flag = case_when(temp.degC > 20 & temp.degC < 25 ~ "SVC",
                             temp.degC < 4 & temp.degC > 0 ~ "SVC",  # OK so long as subtidal only
                             temp.degC > 25 ~ "SVD",
                             temp.degC < 0 ~ "SVD",
                             TRUE ~ "AC"),
         qc_flag = as.factor(qc_flag)) %>%
  # remove any NA temperatures - not all stations have data covering the same periods
  drop_na(temp.degC)

# ---- SAVE CLEANED DATA ----
# save all temperature data with QC flags
write.csv(temperature_flagged, "clean-data/temperature_all.csv")


# ---- PLOT DATA ----
## plot timeseries lengths by station
temperature_flagged %>%
  ggplot(aes(x = dateTime, y = niceSiteNames, col = niceSiteNames)) +
  geom_point(size = 6, show.legend = FALSE) +
  xlab("Date") +
  ylab("Site") +
  labs(title = "Hakai Nearshore Temperature Array",
       subtitle = "Timeseries by Station") +
  theme_classic()

## plot temperature timeseries for single site
# choked interior
temperature_flagged %>%
  filter(station_id == "choked_inner_sgrass") %>%
  ggplot(aes(x = dateTime, y = temp.degC)) +
  geom_point(size = 0.5, alpha = 0.5) +
  # geom_smooth() +
  xlab("Date") +
  ylab("Temperature (ºC)") +
  labs(title = "Choked Interior temperature") +
  theme_classic()