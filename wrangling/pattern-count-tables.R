# Filtered Depth Data
filtered_depth <- tuna %>%
  filter(depth_tuna1pt_qlevel != 1)

# Count occurrences of each pattern in the 'qflag' column
pattern_counts_depth <- table(filtered_depth$depth_tuna1pt_qflag)

# Include 'AV' in the counts
pattern_counts_depth["AV"] <- sum(filtered_depth$depth_tuna1pt_qflag == "AV")
pattern_counts_depth["MV"] <- sum(filtered_depth$depth_lull1pt_qflag == "MV")
pattern_counts_depth["EV"] <- sum(filtered_depth$depth_lull1pt_qflag == "EV")
pattern_counts_depth["SVD"] <- sum(filtered_depth$depth_lull1pt_qflag == "SVD")
pattern_counts_depth["SVC"] <- sum(filtered_depth$depth_lull1pt_qflag == "SVC")

# Add a row for the total number of records
pattern_counts_depth <- rbind(pattern_counts_depth, Total = sum(pattern_counts_depth))

# Display the pattern counts for depth
print("Pattern Counts for Depth:")
print(pattern_counts_depth)


# Filtered Temperature Data
filtered_temp <- tuna %>%
  filter(twtr_tuna1pt_qlevel != 1)

# Count occurrences of each pattern in the 'qflag' column
pattern_counts_temp <- table(filtered_temp$twtr_lull1pt_qflag)

# Include 'AV' in the counts
pattern_counts_temp["AV"] <- sum(filtered_temp$twtr_lull1pt_qflag == "AV")
pattern_counts_temp["MV"] <- sum(filtered_temp$twtr_lull1pt_qflag == "MV")
pattern_counts_temp["EV"] <- sum(filtered_temp$twtr_lull1pt_qflag == "EV")
pattern_counts_temp["SVD"] <- sum(filtered_temp$twtr_lull1pt_qflag == "SVD")
pattern_counts_temp["SVC"] <- sum(filtered_temp$twtr_lull1pt_qflag == "SVC")

# Add a row for the total number of records
pattern_counts_temp <- rbind(pattern_counts_temp, Total = sum(pattern_counts_temp))

# Display the pattern counts for temperature
print("Pattern Counts for Temperature:")
print(pattern_counts_temp)


# Filtered Tidbit Data
filtered_tidbit <- tuna %>%
  filter(twtr_tuna1_tb1_qlevel != 1)

# Count occurrences of each pattern in the 'qflag' column
pattern_counts_tidbit <- table(filtered_tidbit$twtr_lull1pt_qflag)

# Include 'AV' in the counts
pattern_counts_tidbit["AV"] <- sum(filtered_tidbit$twtr_lull1_tb1_qflag == "AV")
pattern_counts_tidbit["MV"] <- sum(filtered_tidbit$twtr_lull1_tb1_qflag == "MV")
pattern_counts_tidbit["EV"] <- sum(filtered_tidbit$twtr_lull1_tb1_qflag == "EV")
pattern_counts_tidbit["SVD"] <- sum(filtered_tidbit$twtr_lull1_tb1_qflag == "SVD")
pattern_counts_tidbit["SVC"] <- sum(filtered_tidbit$twtr_lull1_tb1_qflag == "SVC")

# Add a row for the total number of records
pattern_counts_tidbit <- rbind(pattern_counts_tidbit, Total = sum(pattern_counts_tidbit))

# Display the pattern counts for tidbit
print("Pattern Counts for Tidbit:")
print(pattern_counts_tidbit)

#################################################################################################################################################################################
# Combine Pattern Counts for All Variables
combined_pattern_counts <- data.frame(
  Variable = c("Depth", "Temperature", "Tidbit"),
  AV = c(
    sum(filtered_depth$depth_tuna1pt_qflag == "AV"),
    sum(filtered_temp$twtr_tuna1pt_qflag == "AV"),
    sum(filtered_tidbit$twtr_tuna1_tb1_qflag == "AV")
  ),
  MV = c(
    sum(filtered_depth$depth_tuna1pt_qflag == "MV"),
    sum(filtered_temp$twtr_tuna1pt_qflag == "MV"),
    sum(filtered_tidbit$twtr_tuna1_tb1_qflag == "MV")
  ),
  EV = c(
    sum(filtered_depth$depth_tuna1pt_qflag == "EV"),
    sum(filtered_temp$twtr_tuna1pt_qflag == "EV"),
    sum(filtered_tidbit$twtr_tuna1_tb1_qflag == "EV")
  ),
  SVD = c(
    sum(filtered_depth$depth_tuna1pt_qflag == "SVD"),
    sum(filtered_temp$twtr_tuna1pt_qflag == "SVD"),
    sum(filtered_tidbit$twtr_tuna1_tb1_qflag == "SVD")
  ),
  SVC = c(
    sum(filtered_depth$depth_tuna1pt_qflag == "SVC"),
    sum(filtered_temp$twtr_tuna1pt_qflag == "SVC"),
    sum(filtered_tidbit$twtr_tuna1_tb1_qflag == "SVC")
  ),
  Total = c(
    nrow(filtered_depth),
    nrow(filtered_temp),
    nrow(filtered_tidbit)
  )
)

# Print the Combined Table
print("Combined Pattern Counts for All Variables:")
print(combined_pattern_counts)
##########################################################################
#Koeye time-series
#########################################################################


filtered_data <- df %>%
  filter(sensor %in% c("PT", "PT2", "SC", "tempPT", "tempPT2", "tempSC") & qflag %in% c("SVC", "SVD", "EV", "AV"))

result_table <- filtered_data %>%
  group_by(sensor, qflag) %>%
  summarise(count = n())

print(result_table)
