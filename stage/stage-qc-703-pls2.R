library(hakaiApi)
library(readr)
library(tidyr)
library(dplyr)
library(zoo)
library(ggplot2)
library(lubridate)
library(glue)
library(stringr)
library(data.table)

create_qc_table_name <- function(table_name) {
  table_name <- tolower(table_name)
  gsub(":", "_", table_name)
}

qc_author <- 'emily.haughton@hakai.org'
baseurl <- "https://hecate.hakai.org/api"
station_name <- "SSN703US"
sampling_interval <- "5minute"
table_name <- glue("{station_name}:{sampling_interval}")
qc_table_name <- create_qc_table_name(table_name)
meas_start_date <- "2017-11-13"
meas_end_date <- "2021-04-30"
field_names <- glue_collapse(
  c(
    "measurementTime",
    glue("{station_name}:PLS2_Lvl_Avg"),
    glue("{station_name}:PLS2_Lvl_QL"),
    glue("{station_name}:PLS2_Lvl_QC")
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
    str_ends(.x, ":PLS2_Lvl_Avg$") ~ "stage",
    str_ends(.x, ":PLS2_Lvl_QL$") ~ "qlevel", 
    str_ends(.x, ":PLS2_Lvl_QC$") ~ "qflag",
    TRUE ~ .x
  ))

# ---qc wrangling ---


#UTC offset -08:00 for viewing in PST
df <- df %>%
  mutate(
    measurementTime_PST = measurementTime - hours(8)  #8 hours
  )


# Define bad ranges as a data.table #sn graphing
bad_ranges <- data.table(
  start = ymd_hms(c("2019-02-09 00:05:00", 
                    "2019-02-25 08:15:00",
                    "2019-03-03 02:00:00",
                    "2020-03-14 12:00:00")),
  end   = ymd_hms(c("2019-02-09 20:15:00", 
                    "2019-02-28 00:05:00",
                    "2019-03-08 11:00:00",
                    "2020-03-20 18:00:00"))
)

# Convert df to data.table
df_dt <- as.data.table(df)

# Set keys for interval join
setkey(bad_ranges, start, end)

#join to set stage to NA for ice-affected periods
df_dt[bad_ranges, on = .(measurementTime_PST >= start, measurementTime_PST <= end), stage := NA_real_]

df_dt

df_dt <- df_dt[265:.N]  #remove all before row 265 which (.N is all rows)


# Ensure data is ordered
setorder(df_dt, measurementTime_PST)

# identify runs of NAs
df_dt[, na_run := rleid(is.na(stage))]

df_dt[, qlevel := as.character(qlevel)]

# --- Step 2: Identify consecutive NA runs ---
df_dt[, na_run := rleid(is.na(stage))]

gap_info <- df_dt[is.na(stage), .(
  gap_length = .N,
  idx_start = .I[1],
  idx_end = .I[.N]
), by = na_run]

# --- Step 3: Parameters ---
small_gap_max <- 6 * 12  # 6 hours for 5-min data (12 per hour)

# --- Step 4: Fill gaps ---
# compute full-series interpolations
stage_linear <- na.approx(df_dt$stage, x = as.numeric(df_dt$measurementTime_PST), na.rm = FALSE)
stage_spline <- na.spline(df_dt$stage, x = as.numeric(df_dt$measurementTime_PST))

for(i in seq_len(nrow(gap_info))){
  start_idx <- gap_info[i, idx_start]
  end_idx   <- gap_info[i, idx_end]
  gap_len   <- gap_info[i, gap_length]
  
  if(gap_len <= small_gap_max){
    df_dt[start_idx:end_idx, stage := stage_linear[start_idx:end_idx]]
    df_dt[start_idx:end_idx, qflag := "EV: Gap-filled-linear: QC'd by EH"]
    df_dt[start_idx:end_idx, qlevel := "3"]
  } else {
    df_dt[start_idx:end_idx, stage := stage_spline[start_idx:end_idx]]
    df_dt[start_idx:end_idx, qflag := "EV: Gap-filled-spline: QC'd by EH"]
    df_dt[start_idx:end_idx, qlevel := "3"]
  }
}

# --- Step 5: Flag accepted values ---
df_dt[!is.na(stage) & is.na(qflag), `:=`(
  qflag  = "AV: QC'd by EH",
  qlevel = "2"
)]

# Clean up columns
df_dt[, na_run := NULL]

df_dt

#plot and examine
library(plotly)
library(ggplot2)
# Create a simple flag for EV vs original
df_dt[, is_ev := ifelse(grepl("^EV", qflag), "EV", "Original")]

setDF(df_dt)#convert data table to dataframe


# Base ggplot
p <- ggplot(df_dt, aes(x = measurementTime_PST, y = stage, color = is_ev, text = qflag)) +
  geom_line() +
  scale_color_manual(values = c("Original" = "black", "EV" = "red")) +
  labs(
    title = "Water Level Time Series",
    x = "Measurement Time (PST)",
    y = "Stage (m)",
    color = "QC Status"
  ) +
  theme_minimal()

# Interactive plot
ggplotly(p, tooltip = c("x", "y", "text"))


# --- Step 6: Format and upload to database ---

# Create output df with only the required columns
output_df <- df_dt %>%
  select(measurementTime, qlevel, qflag, stage)

# format output for sending to SN QC
output_df <- output_df %>%
  rename(
    measurement_time = measurementTime,
    quality_level = qlevel,
    qc_flag = qflag,
    val = stage,
  )

output_df['measurement_name'] <- 'PLS2_Lvl'
output_df['qc_by'] <- qc_author
output_df['recorded_time'] <- strftime(lubridate::now() , "%Y-%m-%dT%H:%M:%S%z")
output_df$measurement_time <- strftime(output_df$measurement_time, "%Y-%m-%dT%H:%M:%S%z")

#confirm measurement time format
str(output_df)


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

