data <- c(10, 15, 21, 45, 55, 99)

safety <- 15
l <- length(data)
zn <- rep(0, l)

for (i in 1:l) {
  gauge <- data[i]
  others <- rep(0, l - 1)
  m <- 1
  
  for (j in 1:l) {
    if (j != i) {
      others[m] <- data[j]
      m <- m + 1
    }
  }
  
  mn <- median(others)
  Mn <- mean(abs(others - mn))
  
  # Assuming you want to store the results in the zn vector
  zn[i] <- Mn
}




# Print the results
cat("Results:\n")
cat("Data:", data, "\n")
cat("Mean Absolute Deviation for each point:", zn, "\n")
##########################################################################################################################################


TWtrSSN626US_Q_level	TWtrSSN626US_Q_flags	TWtrSSN626US_Q_flags_extended	TWtrSSN626US_Avg

# Specify the row numbers you want to remove
rows_to_remove <- c(525733:525736)

# Remove specified rows using indexing
lull <- lull[-rows_to_remove, ]


SV <- c("SVD", "SVC")
SV_depth <- sum(grepl(paste(SV, collapse = "|"), depth$depth_lull1pt_qflag))


write.csv(df,"glendale-timeseries.csv",row.names = FALSE)
