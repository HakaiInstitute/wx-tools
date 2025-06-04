library(hakaiApi)
library(readr)
library(tidyr)
library(dplyr)
library(zoo)
library(ggplot2)
library(lubridate)

baseurl = "https://goose.hakai.org/api"



# Initialize the client
setwd("~")
hakai_client <- hakaiApi::Client$new(baseurl)

# Step 1: Load the data
## specify your types via a readr col_spec
cols_type_spec <- cols(
  .default = col_double(),
  measurementTime = col_datetime(format = ""),
  `SSN819US:PLS2_Lvl_QC` = col_character(),
  `SSN819US:PLS2_Temp_QC` = col_character(),
  `SSN819US:PLS_DischargeRate_QC` = col_character(),
  `SSN819US:DischargeRate_QC` = col_character(),
  `SSN819US:DischargeVolume_QC` = col_character(),
  `SSN819US:Turbidity_QC` = col_character(),
  `SSN819US:TurbidityNTU_QC` = col_character()
)

url <- "https://goose.hakai.org/api/sn/views/SSN819US:5minuteSamples?measurementTime>=2016-04-01&measurementTime<2016-04-28"

# Get the table name from api url for later use
tableName <- tolower(gsub("(outlet|trib)", "_\\1", url, ignore.case = TRUE)) %>%
  gsub(".*/", "", .) %>%
  gsub("samples.*$", "", .) %>%
  gsub(":", "_", .)

df <- hakai_client$get(url, col_types = cols_type_spec)

df <- df %>%
  rename(
    stage = "SSN819US:PLS2_Lvl",
    qlevel = "SSN819US:PLS2_Lvl_QL",
    qflag = "SSN819US:PLS2_Lvl_QC"
  )

# Step 2: Apply linear interpolation for small gaps (<12)
df <- df %>%
  arrange(measurementTime) %>%
  mutate(
    stage_filled = zoo::na.approx(stage, x = measurementTime, maxgap = 12, na.rm = FALSE),
    qflag = case_when(
      is.na(stage) & !is.na(stage_filled) ~ "EV_LINEAR",  # Linear fill
      TRUE ~ qflag
    )
  )

# Step 3: Identify NA groups and calculate gap lengths
df <- df %>%
  mutate(is_na = is.na(stage)) %>%
  mutate(group_id = cumsum(!is_na))  # Create NA group IDs

# Summarize NA groups
na_groups <- df %>%
  filter(is_na) %>%
  group_by(group_id) %>%
  summarise(
    start_time = min(measurementTime),
    end_time = max(measurementTime),
    .groups = "drop"
  ) %>%
  mutate(gap_minutes = as.numeric(difftime(end_time, start_time, units = "mins")))

# Step 4: Identify gaps for spline (60 < gap ≤ 380 minutes)
df <- df %>%
  left_join(na_groups %>% select(group_id, gap_minutes), by = "group_id") %>%
  mutate(
    is_large_gap = gap_minutes > 60 & gap_minutes <= 380,
    is_very_large_gap = gap_minutes > 380,
    qflag = case_when(
      is.na(stage_filled) & is_very_large_gap ~ "MV",  # Too large to fill
      TRUE ~ qflag
    )
  )

# Step 5: Full-series spline interpolation
spline_interp <- as.data.frame(
  spline(
    x = as.numeric(df$measurementTime),
    y = df$stage,
    xout = as.numeric(df$measurementTime)
  )
)
colnames(spline_interp) <- c("timestamp_numeric", "spline_stage")
spline_interp$measurementTime <- as.POSIXct(spline_interp$timestamp_numeric, origin = "1970-01-01", tz = "UTC")

# Join spline to main df
df <- df %>%
  left_join(spline_interp %>% select(measurementTime, spline_stage), by = "measurementTime")

# Step 6: Fill large gaps with spline
df <- df %>%
  mutate(
    stage_filled = if_else(
      is.na(stage_filled) & is_large_gap & !is.na(spline_stage),
      spline_stage,
      stage_filled
    ),
    qflag = case_when(
      is.na(stage) & is_large_gap & !is.na(stage_filled) ~ "EV_SPLINE",
      TRUE ~ qflag
    )
  )

# Step 7: Apply manual update to a subset range
subset_df <- df %>%
  filter(measurementTime >= as.POSIXct("2025-05-06 07:00:00") & measurementTime <= as.POSIXct("2025-05-09 00:00:00"))

# Step 7.2: Override flags only for the subset
subset_df <- subset_df %>%
  mutate(
    stage_filled = if_else(
      !is.na(spline_stage), spline_stage, stage_filled
    ),
    qflag = case_when(
      !is.na(spline_stage) ~ "EV_SPLINE",
      is.na(spline_stage) & !is.na(stage_filled) ~ "AV",
      TRUE ~ qflag
    )
  )

# Step 7.3: Merge updated subset back into full dataset
final_df <- df %>%
  left_join(subset_df %>% select(measurementTime, qflag, stage_filled), by = "measurementTime", suffix = c("", "_updated")) %>%
  mutate(
    stage_filled = coalesce(stage_filled_updated, stage_filled),
    qflag = coalesce(qflag_updated, qflag)
  ) %>%
  select(-ends_with("_updated"))

# Step 8: Apply final qflag and qlevel logic 
final_df <- final_df %>%
  mutate(
    qflag = case_when(
      is.na(qflag) & !is.na(stage_filled) ~ "AV",
      TRUE ~ qflag
    ),
    qlevel = case_when(
      qflag == "AV" ~ 2,
      qflag %in% c("EV_LINEAR", "EV_SPLINE") ~ 3,
      qflag == "MV" ~ 2,
      TRUE ~ qlevel
    )
  )

# Step 9: Add year column for plotting
final_df <- final_df %>%
  mutate(year = format(measurementTime, "%Y"))

# Step 10: Plot the results
ggplot(final_df, aes(x = measurementTime)) +
  geom_line(aes(y = stage_filled), color = "black", size = 0.5, alpha = 0.8) +
  geom_point(aes(y = stage_filled, color = qflag, shape = qflag), size = 3, alpha = 0.8) +
  scale_color_manual(values = c(
    "EV_SPLINE" = "purple",
    "EV_LINEAR" = "blue",
    "AV" = "grey70",
    "MV" = "red"
  )) +
  scale_shape_manual(values = c(
    "EV_SPLINE" = 2,
    "EV_LINEAR" = 1,
    "AV" = 16,
    "MV" = 17
  )) +
  scale_x_datetime(date_labels = "%b %d", date_breaks = "2 days") +
  scale_y_continuous(name = "Water Level (m)") +
  labs(
    title = "Water Level QC Overview",
    subtitle = "Black = Final Line, Purple = Spline Fill, Blue = Linear Fill, Grey = AV, Red = MV (Long Gap)",
    x = "Date", y = "Water Level"
  ) +
  theme_minimal(base_size = 12) +
  facet_wrap(~year, scales = "free_x", ncol = 1)

# Create output df with only the required columns
output_df <- final_df %>%
  select(measurementTime, qlevel, qflag, stage_filled)

# format output for sending to SN QC
output_df <- output_df %>%
  rename(
    measurement_time = measurementTime,
    quality_level = qlevel,
    qc_flag = qflag,
    val = stage_filled,
  )

output_df['measurement_name'] <- 'PLS_Lvl'
output_df['qc_by'] <- 'emily.haughton@hakai.org'
output_df['recorded_time'] <- strftime(lubridate::now() , "%Y-%m-%dT%H:%M:%S%z")
output_df$measurement_time <- strftime(output_df$measurement_time, "%Y-%m-%dT%H:%M:%S%z")

# Send QC data back to the database (1000 rows at a time)
QCURL = paste0(baseurl, "/sn/qc/", tableName)
cat('Sending PATCH requests to:', QCURL)
window_size <- 1000
for (i in 0:(nrow(output_df) %/% window_size)) {
  print(i)
  print(c(i*window_size+1, min(nrow(output_df), (i + 1) * window_size)))
  lb <- i*window_size+1
  ub <- min(nrow(output_df), (i + 1) * window_size)
  hakai_client$patch(QCURL,output_df[lb:ub,])
}


