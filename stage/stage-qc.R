library(hakaiApi)
library(readr)
library(tidyr)
library(dplyr)
library(zoo)
library(ggplot2)
library(lubridate)
library(glue)
library(stringr)

create_qc_table_name <- function(table_name) {
  table_name <- tolower(table_name)
  gsub("us:", "_", table_name)
}

qc_author <- 'emily.haughton@hakai.org'
baseurl <- "https://hecate.hakai.org/api"
station_name <- "SSN844US"
sampling_interval <- "5minute"
table_name <- glue("{station_name}:{sampling_interval}")
qc_table_name <- create_qc_table_name(table_name)
meas_start_date <- "2023-06-01"
meas_end_date <- "2023-06-30"
field_names <- glue_collapse(
  c(
    "measurementTime",
    glue("{station_name}:PLS_Lvl"),
    glue("{station_name}:PLS_Lvl_QL"),
    glue("{station_name}:PLS_Lvl_QC")
  ),
  sep = ","
)

# Initialize the client
hakai_client <- hakaiApi::Client$new(baseurl)

# Step 1: Load the data
## construct the url
url <- glue("{baseurl}/sn/views/{table_name}Samples?measurementTime>={meas_start_date}&measurementTime<{meas_end_date}&fields={field_names}&limit=-1")
df_raw <- hakai_client$get(url)

## set types explicitly
df <- df_raw |> 
  mutate(across(ends_with("_QC"), as.character)) |> 
  mutate(across(!ends_with("_QC") & !any_of("measurementTime"), as.double)) |> 
  mutate(measurementTime = as_datetime(measurementTime))

## generic column renaming
df <- df %>%
  rename_with(~ case_when(
    str_ends(.x, ":PLS_Lvl$") ~ "stage",
    str_ends(.x, ":PLS_Lvl_QL$") ~ "qlevel", 
    str_ends(.x, ":PLS_Lvl_QC$") ~ "qflag",
    TRUE ~ .x
  ))
##########################################################################################################################################################
#qc visualization
##########################################################################################################################################################
#subset df
start_time<- as.POSIXct("2022-10-01 00:00", tz="America/Los_Angeles")
end_time<-as.POSIXct("2025-10-01 00:00", tz="America/Los_Angeles")

df_2025<-df %>% 
  filter(measurementTime >=start_time&measurementTime<=end_time)

#specify qc measurement range
start_time_qc<- as.POSIXct("2023-06-05 07:35", tz="America/Los_Angeles")
end_time_qc<-as.POSIXct("2023-06-11 17:25", tz="America/Los_Angeles")

df_2025_qc<-df_2025 %>% 
  mutate(stage=ifelse(measurementTime >=start_time_qc&measurementTime<=end_time_qc, NA, stage))

plot1<-ggplot(df, aes(x=measurementTime, y=stage))+
  geom_line(colour="steelblue", size=0.5) +
  geom_point(colour="darkblue")+
  labs(x = "Measurement Time",
       y = "Stage (m)")+
  theme_minimal() 

plot1

###############################
df_2025_qc <- df_2025_qc %>%
  mutate(
    year = year(measurementTime),
    stage_status = ifelse(is.na(stage), "Missing", "Measured")
  )

# Plot
ggplot(df_2025_qc, aes(x = measurementTime, y = stage)) +
  # Line for measured (non-NA) values
  geom_line(data = subset(df_2025_qc, !is.na(stage)), aes(color = "Measured")) +
  
  # Points for missing (NA) values
  geom_point(data = subset(df_2025_qc, is.na(stage)), aes(color = "Missing"), size = 0.7) +
  
  scale_color_manual(values = c("Measured" = "blue", "Missing" = "red")) +
  
  facet_wrap(~year, scales = "free_x") +
  
  labs(title = "Time Series of Stage by Year",
       x = "Date",
       y = "Stage",
       color = "Data Status") +
  
  theme_minimal()

#investigate missing data
df_2025_qc %>%
  mutate(year = year(measurementTime)) %>%
  group_by(year) %>%
  summarize(missing_stage_count = sum(is.na(stage)),
            total = n(),
            percent_missing = round(100 * missing_stage_count / total, 2))

#plot with geom_rect
library(dplyr)
library(ggplot2)
library(lubridate)

# Step 1: Identify missing periods
df_2025_qc <- df_2025_qc %>%
  arrange(measurementTime) %>%
  mutate(
    is_missing = is.na(stage),
    time_diff = as.numeric(difftime(measurementTime, lag(measurementTime), units = "hours")),
    gap_flag = ifelse(is.na(stage) & !is.na(lag(stage)), TRUE, FALSE),
    year = year(measurementTime)
  )

# Step 2: Create a group ID for each run of missing values
df_2025_qc$gap_group <- cumsum(!df_2025_qc$is_missing & (is.na(lag(df_2025_qc$is_missing)) | lag(df_2025_qc$is_missing) == TRUE))

# Step 3: Extract continuous missing blocks
missing_blocks <- df_2025_qc %>%
  filter(is_missing) %>%
  group_by(gap_group) %>%
  summarize(
    start_time = min(measurementTime),
    end_time = max(measurementTime),
    year = first(year)
  ) %>%
  ungroup()

# Step 4: Plot with shaded gaps
ggplot() +
  # Background shaded gap rectangles
  geom_rect(data = missing_blocks,
            aes(xmin = start_time, xmax = end_time, ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.2) +
  
  # Actual measured data
  geom_line(data = filter(df_2025_qc, !is.na(stage)),
            aes(x = measurementTime, y = stage),
            color = "blue") +
  
  facet_wrap(~year, scales = "free_x") +
  labs(
    title = "Time Series of Stage with Missing Data Gaps Highlighted",
    x = "Date", y = "Stage"
  ) +
  theme_minimal()
## gap fill
library(dplyr)
library(zoo)

df_filled <- df_2025_qc %>%
  arrange(measurementTime) %>%
  mutate(
    # Interpolate missing stage values
    stage_filled = na.approx(stage, x = measurementTime, na.rm = FALSE),
    
    # Identify which rows were filled
    was_gap_filled = is.na(stage) & !is.na(stage_filled),
    
    # Replace stage with filled values
    stage = stage_filled,
    
    # Update qflag and qlevel where filling occurred
    qflag = ifelse(was_gap_filled, "EV: gap-filled", qflag),
    qlevel = ifelse(was_gap_filled, 2, qlevel)
  ) %>%
  select(-stage_filled, -was_gap_filled)


#check # of fills
df_filled %>% filter(qflag == "EV: gap-filled")

#plot new dataset
library(ggplot2)
library(dplyr)
library(lubridate)

# Assume df_filled contains interpolated data with updated qflag
df_filled <- df_filled %>%
  arrange(measurementTime) %>%
  mutate(
    year = year(measurementTime),
    is_gap_filled = qflag == "EV: gap-filled"
  )

# Step 1: Identify continuous runs of gap-filled values
df_filled$fill_group <- with(rle(df_filled$is_gap_filled), 
                             rep(seq_along(values), lengths))

# Step 2: Summarize rectangles for gap-filled runs only
gap_filled_rects <- df_filled %>%
  filter(is_gap_filled) %>%
  group_by(fill_group, year) %>%
  summarize(
    start_time = min(measurementTime),
    end_time = max(measurementTime),
    .groups = "drop"
  )

# Step 3: Plot
ggplot() +
  # Shade gap-filled sections
  geom_rect(data = gap_filled_rects,
            aes(xmin = start_time, xmax = end_time, ymin = -Inf, ymax = Inf),
            fill = "orange", alpha = 0.2) +
  
  # Stage time series line
  geom_line(data = df_filled,
            aes(x = measurementTime, y = stage),
            color = "blue") +
  
  facet_wrap(~year, scales = "free_x") +
  labs(
    title = "Stage Time Series with Gap-Filled Sections Highlighted",
    x = "Date", y = "Stage"
  ) +
  theme_minimal()


##########################################################################################################################################################


# Step 2: Apply linear interpolation for small gaps (<12)
df <- df %>%
  arrange(measurementTime) %>%
  mutate(
    stage_filled = zoo::na.approx(stage, x = measurementTime, maxgap = 12, na.rm = FALSE),
    qflag = case_when(
      is.na(stage) & !is.na(stage_filled) ~ "EV_LINEAR",  # Linear fill
      TRUE ~ as.character(qflag)
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
output_df <- df_filled %>%
  select(measurementTime, qlevel, qflag, stage)

# format output for sending to SN QC
output_df <- output_df %>%
  rename(
    measurement_time = measurementTime,
    quality_level = qlevel,
    qc_flag = qflag,
    val = stage,
  )

output_df['measurement_name'] <- 'PLS_Lvl'
output_df['qc_by'] <- qc_author
output_df['recorded_time'] <- strftime(lubridate::now() , "%Y-%m-%dT%H:%M:%S%z")
output_df$measurement_time <- strftime(output_df$measurement_time, "%Y-%m-%dT%H:%M:%S%z")


# Send QC data back to the database (1000 rows at a time)
QCURL = paste0(baseurl, "/sn/qc/", qc_table_name)
cat('Sending PATCH requests to:', QCURL)
window_size <- 1000
for (i in 0:(nrow(output_df) %/% window_size)) {
  print(i)
  print(c(i*window_size+1, min(nrow(output_df), (i + 1) * window_size)))
  lb <- i*window_size+1
  ub <- min(nrow(output_df), (i + 1) * window_size)
  hakai_client$patch(QCURL,output_df[lb:ub,])
}


