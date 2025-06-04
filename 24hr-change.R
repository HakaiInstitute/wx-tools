# Load necessary libraries
library(dplyr)
library(lubridate)

# Assuming df_twtr is your existing dataset

# Extract the site names directly from the column headers (assuming they all start with twtr)
site_names <- grep("^twtr", colnames(df_twtr), value = TRUE)

# Add a date column to group data by each day (midnight to midnight)
df_twtr <- df_twtr %>%
  mutate(date = floor_date(timestamp, unit = "day"))

# Initialize a dataframe to store the range calculations
df_temp_range <- data.frame(date = unique(df_twtr$date))

# Calculate the 24-hour temperature range, hourly range, and prepare for flagging
for (site in site_names) {
  # Calculate the range for each site and merge into df_temp_range
  temp_range <- df_twtr %>%
    group_by(date) %>%
    summarise(
      Range_24h = ifelse(all(is.na(.data[[site]])), NA, 
                         max(.data[[site]], na.rm = TRUE) - min(.data[[site]], na.rm = TRUE)),
      Range_Hourly = Range_24h / 24,
      Range_Multiplied = Range_Hourly * 4,
      .groups = "drop"  # Avoid warnings related to grouping
    )
  
  # Rename the columns to include site name
  colnames(temp_range) <- c("date", 
                            paste0(site, "_Range_24h"), 
                            paste0(site, "_Range_Hourly"), 
                            paste0(site, "_Range_Multiplied"))
  
  # Merge the calculated ranges into the df_temp_range dataframe
  df_temp_range <- left_join(df_temp_range, temp_range, by = "date")
}

# Now join the temp range calculations back to the original dataframe
df_twtr <- df_twtr %>%
  left_join(df_temp_range, by = "date")

# Fix any potential column conflicts (i.e., remove .x suffixes)
colnames(df_twtr) <- gsub("\\.x$", "", colnames(df_twtr))

# Print the column names after merging to check for changes
print(colnames(df_twtr))

# Add flag columns for each site based on the difference from the previous temperature value
df_twtr <- df_twtr %>%
  arrange(timestamp)  # Ensure the data is ordered by timestamp

for (site in site_names) {
  df_twtr <- df_twtr %>%
    mutate(
      !!paste0(site, "_Flag") := ifelse(
        abs(.data[[site]] - lag(.data[[site]])) > (8 * .data[[paste0(site, "_Range_Hourly")]]), 
        "SVC", 
        NA
      )
    )
}

# Ungroup after flagging
df_twtr <- df_twtr %>%
  ungroup()

# Create a new dataframe with the selected and reordered columns
new_df_ordered <- df_twtr %>%
  select(timestamp, all_of(site_names), ends_with("_Flag")) %>%
  select(timestamp, unlist(lapply(site_names, function(site) {
    c(site, paste0(site, "_Flag"))
  })))

# Preview the newly ordered dataframe
print(new_df_ordered)

#
# Filter out records with "SVC" flags in any flag column
svc_records <- new_df_ordered %>%
  filter(if_any(ends_with("_Flag"), ~ . == "SVC"))

# Preview the records with SVC flags
print(svc_records)

