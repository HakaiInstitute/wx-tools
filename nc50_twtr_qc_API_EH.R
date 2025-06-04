###############################################################################################################################################
#standard qc checks
###############################################################################################################################################
library(dplyr)

# Define acceptable range for stream temperature
min_temp <- -5
max_temp <- 40
max_change <- 2  # Maximum allowed change between consecutive timestamps

# Helper function to determine if the first non-NA value is at 0:00
is_first_non_na_at_midnight <- function(timestamp_col, value_col) {
  first_non_na_index <- which(!is.na(value_col))[1]
  if (!is.na(first_non_na_index)) {
    return(format(timestamp_col[first_non_na_index], "%H:%M") == "00:00")
  }
  return(FALSE)
}


check_range_and_change <- function(value_col, timestamp_col) {
  flags <- rep("AV", length(value_col))
  ql <- rep(2, length(value_col))
  uql <- rep(1, length(value_col))
  
  # NA check
  if (!is_first_non_na_at_midnight(timestamp_col, value_col)) {
    non_na_indices <- which(!is.na(value_col))
    if (length(non_na_indices) > 0) {
      flags[non_na_indices[1:min(15, length(non_na_indices))]] <- "SVD: Predeployment: QC'd by EH"
      ql[non_na_indices[1:min(15, length(non_na_indices))]] <- 2
      uql[non_na_indices[1:min(15, length(non_na_indices))]] <- 4
    }
  }
  
  # Range check
  flags[value_col < min_temp | value_col > max_temp] <- paste(
      sep=',', 
      flags[value_col < min_temp | value_col > max_temp], 
      "Range Check: Out of Bounds: QC'd by EH"
    )
  ql[value_col < min_temp | value_col > max_temp] <- 2
  uql[value_col < min_temp | value_col > max_temp] <- max(c(uql[value_col < min_temp | value_col > max_temp], 3))
  
  # Change check
  temp_diff <- c(NA, diff(value_col))
  flags[!is.na(temp_diff) & abs(temp_diff) > max_change] <- paste(
      sep=',', 
      flags[!is.na(temp_diff) & abs(temp_diff) > max_change], 
      "Change Check: Exceeds Max Change: QC'd by EH"
    )
  ql[!is.na(temp_diff) & abs(temp_diff) > max_change] <- 2
  uql[!is.na(temp_diff) & abs(temp_diff) > max_change] <- max(c(uql[!is.na(temp_diff) & abs(temp_diff) > max_change], 3))
  
  return(cbind(flags,ql,uql))
}


df_twtr_api[, c("watertemp_qc","watertemp_ql","watertemp_uql")] <- check_range_and_change(df_twtr_api$watertemp, df_twtr_api$timestamp)
View(df_twtr_api)

# Assign na to watertemps flaged with SVD
df_twtr_api[grepl("^SVD", df_twtr_api$watertemp_qc), ]$watertemp <- NA

# write QC data back to database

df_twtr_api<- df_twtr_api[,1:3]
colnames(df_twtr_api) <- c('measurement_time', 'quality_level', 'qc_flag')
df_twtr_api['measurement_name'] <- 'WaterTemp'
df_twtr_api['qc_by'] <- 'me.here@hakai.org'
df_twtr_api['recorded_time'] <- strftime(now() , "%Y-%m-%dT%H:%M:%S%z")
df_twtr_api$measurement_time <- strftime(df_twtr_api$measurement_time, "%Y-%m-%dT%H:%M:%S%z")

# INSERT ONLY qc rows into the database in groups of 1000 rows at a time
baseurl = paste0("https://goose.hakai.org/api/sn/qc/",tableName)
cat('Sending POST requests to:', baseurl)
window_size <- 1000
for (i in 0:(nrow(df_twtr_api) %/% window_size)) {
  print(i)
  print(c(i*window_size+1, min(nrow(df_twtr_api), (i + 1) * window_size)))
  lb <- i*window_size+1
  ub <- min(nrow(df_twtr_api), (i + 1) * window_size)
  client$post(baseurl,df_twtr_api[lb:ub,])
}

# OR

# INSERT or UPDATE qc rows in the database using upsert in groups of 1000 rows at a time
baseurl = paste0("https://goose.hakai.org/api/sn/qc/",tableName)
cat('Sending PATCH requests to:', baseurl)
window_size <- 1000
for (i in 0:(nrow(df_twtr_api) %/% window_size)) {
  print(i)
  print(c(i*window_size+1, min(nrow(df_twtr_api), (i + 1) * window_size)))
  lb <- i*window_size+1
  ub <- min(nrow(df_twtr_api), (i + 1) * window_size)
  client$patch(baseurl,df_twtr_api[lb:ub,])
}

#get serial number 
serial<-client$get('https://goose.hakai.org/api/sn/tables/measurements?database_table=sa_tuna1_tb1_5minute&database_column=water_temp_avg')
