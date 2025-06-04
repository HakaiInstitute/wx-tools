# Load necessary library
library(ggplot2)
library(dplyr)

# Sample dataset (Replace this with your actual data)
data <- data.frame(
  Site = c("10002_outlet1", "10622_outlet1", "5869_outlet1", "adam_trib6", "punt_outlet1", "tren_outlet1", "tsol_outlet2", "wick_outlet1"),
  Pass = c(29925, 29694, 19931, 31415, 31269, 35722, 29712, 11498),
  Fail = c(0, 227, 336, 596, 986, 1173, 2493, 703)
)

# Transform data to long format for ggplot
data_long <- data %>%
  tidyr::pivot_longer(cols = c(Pass, Fail), names_to = "Result", values_to = "Count")

# Plot
ggplot(data_long, aes(x = reorder(site, Count), y = Count, fill = Result)) +
  geom_bar(stat = "identity", position = "stack") +
  coord_flip() +  # Flip axes for better readability
  scale_fill_manual(values = c("Pass" = "steelblue", "Fail" = "red")) +
  labs(title = "Data Quality Assessment by Site",
       x = "Site",
       y = "Count",
       fill = "Result") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10), legend.position = "top")

#########################################################################
#pie chart
# Load libraries
library(ggplot2)

# Aggregate data
total_pass <- sum(data$Pass)
total_fail <- sum(data$Fail)

# Create aggregated data frame
agg_data <- data.frame(
  Result = c("Pass", "Fail"),
  Count = c(total_pass, total_fail)
)

# Plot
ggplot(agg_data, aes(x = "", y = Count, fill = Result)) +
  geom_bar(stat = "identity", width = 0.5) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("Pass" = "steelblue", "Fail" = "red")) +
  labs(title = "Overall Data Quality Assessment",
       fill = "Result") +
  theme_void() +
  theme(legend.position = "right")
#########################################################################
#Pass rate
# Calculate pass rate
pass_rate <- (total_pass / (total_pass + total_fail)) * 100

# Display results
summary_data <- data.frame(
  Metric = c("Total Pass", "Total Fail", "Pass Rate (%)"),
  Value = c(total_pass, total_fail, round(pass_rate, 2))
)
print(summary_data)
#########################################################################
# Load libraries
library(ggplot2)

# Load libraries
library(ggplot2)

# Aggregate data
total_pass <- sum(data$Pass)
total_fail <- sum(data$Fail)

# Create aggregated data frame
agg_data <- data.frame(
  Result = c("Pass", "Fail"),
  Count = c(total_pass, total_fail)
)

# Plot
ggplot(agg_data, aes(x = "", y = Count, fill = Result)) +
  geom_bar(stat = "identity", width = 0.5) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("Pass" = "steelblue", "Fail" = "red")) +
  labs(title = "Overall Data Quality Assessment",
       fill = "Result") +
  theme_void() +
  theme(legend.position = "right")
