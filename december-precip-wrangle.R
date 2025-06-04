# Assuming your dataset is named 'rain_data' with columns 'date' and 'rain'
# You may need to load your dataset or create it accordingly

# Convert 'date' column to Date type if it's not already in that format
rain_data$date <- as.Date(rain_data$date)

# Extract the year and month from the 'date' column
df$year <- format(df$date, "%Y")
df$month <- format(df$date, "%m")

# Filter data for December (month == '12')
december_data <- df[df$month == '12', ]


#pivot
result_df <- pivot_longer(december_data, -c(date, year, month, wateryear), 
                          names_to = c("variable","site","rain"), 
                          names_sep = "\\.") 
  
  
#totals by site
totals_by_site <- result_df %>%
  group_by(year, site) %>%
  summarize(total_value = sum(value, na.rm = TRUE))

# Print or use the 'totals_by_site' data frame as needed
print(totals_by_site)



# Load library
library(ggplot2)

# Assuming your totals_by_site data frame is already available
# Convert 'year' to a factor for proper ordering in the plot
df2$year <- as.factor(df2$year)

# Create a boxplot
ggplot(df2, aes(x = year, y = total_value, fill = site)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Bar Graph of Total Values for Each Site (December for Each Year)",
       x = "Year",
       y = "Total Value") +
  theme_minimal()


df2 <- totals_by_site %>%
  filter(site == "pruth" | site == "quadra")


ggplot(df2, aes(x = year, y = total_value, group = year, color = site)) +
  geom_line() +
  labs(title = "Interannual Precipitation Patterns in December",
       x = "Year",
       y = "Precipitation") +
  theme_minimal()
