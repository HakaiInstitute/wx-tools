# Load necessary libraries
library(ggplot2)
library(dplyr)
library(viridis)
data<-df
data$timestamp <- as.POSIXct(data$timestamp, format="%Y-%m-%d %H:%M", tz="UTC")

#########################################################################################################################
#temp plot all tribs +highlight
# Reshape the data to long format for easier plotting
data_long <- data %>%
  pivot_longer(cols = starts_with("Salm"), names_to = "site", values_to = "Temperature") %>%
  pivot_longer(cols = starts_with("Rain"), names_to = "rain_site", values_to = "Rain") %>%
  mutate(site = gsub("Salm", "", site), rain_site = gsub("Rain_", "", rain_site))

# Define different datetime ranges for each site
# date_ranges <- data.frame(
#   site = Trib5_T1_Avg,
#   start_datetime = as.POSIXct(c("2024-08-24 14:00:00")),#, "2023-06-01 00:00:00", "2023-04-01 00:00:00", "2023-07-01 00:00:00", "2023-08-15 00:00:00")),
#   end_datetime = as.POSIXct(c("2024-10-09 11:00:00")) #, "2023-08-31 23:59:59", "2023-05-01 23:59:59", "2023-09-01 23:59:59", "2023-09-30 23:59:59"))
# )

highlight_site <- "Trib5_T1_Avg"  # Specify the site to highlight (e.g., Site_1)
start_datetime <- as.POSIXct("2024-09-01 14:00:00")
end_datetime <- as.POSIXct("2024-09-31 11:00:00")

# Add Date_Range column only for the specified site
data_long <- data_long %>%
  mutate(Date_Range = ifelse(site == highlight_site & timestamp >= start_datetime & timestamp <= end_datetime, "SVD: Dewatered", "Normal"))

# Define colors for each site
#site_colors <- c("Trib5_T1_Avg" = "blue", "Trib4_T1_Avg" = "green", "Trib4_T1_Avg" = "purple", "Trib2_T1_Avg" = "orange", "Trib6_T1_Avg" = "brown")
# Generate color-blind friendly palette using viridis (for 5 sites)
site_colors <- viridis::viridis(6)  # Generate 5 distinct colors for the sites


# Plot 1: Temperature with highlighted portion for one site
gg_temp <- ggplot(data_long, aes(x = timestamp, y = SalmTrib5_T1_Avg, color = SalmTrib5_T1_Avg, 
                                 text = paste("Date", timestamp))) +
  geom_line(size = 0.5) +  # Line plot for temperature
  geom_line(data = subset(data, Date_Range == "SVD: Dewatered"), aes(color = "SVD: Dewatered"), size = 1) +  # Highlighted range
  scale_color_manual(values = c(setNames(site_colors, c("Trib5_T1_Avg", "Trib4_T1_Avg", "Trib3_T1_Avg", "Trib2_T1_Avg", "Trib1_T1_Avg", "Trib6_T1_Avg")), 
                                "SVD: Dewatered" = "orange")) +
# Single scale for both sites and highlighted portion
  labs(title = "",
       x = "Date", 
       y = "Temperature (°C)") +
  theme_minimal(base_size = 16) +  # Base size for overall readability
  theme(
    axis.title.x = element_text(size = 22, face = "bold"),  # Larger, bold x-axis title
    axis.title.y = element_text(size = 22, face = "bold"),  # Larger, bold y-axis title
    axis.text.x = element_text(size = 16, angle = 0, hjust = 0.5),  # Horizontal x-axis text
    axis.text.y = element_text(size = 16),  # Size 16 for y-axis text
    plot.title = element_text(size = 24, face = "bold"),  # Larger, bold title
    legend.title = element_text(size = 18),  # Legend title size
    legend.text = element_text(size = 16),  # Legend text size
    panel.spacing = unit(1, "lines")  # Space between elements if needed
  )

# Convert ggplot to an interactive plotly plot
interactive_temp_plot <- ggplotly(gg_temp, tooltip = "text")

# Display the interactive plot
interactive_temp_plot
# Save the ggplot as a PNG file with high resolution
ggsave("temp_plot.png", plot = gg_temp, width = 10, height = 6, dpi = 300)

#######################################################################################################################################################
#rain
# Create the ggplot object
gg_rain <- ggplot(data, aes(x = timestamp, y = Rain)) +
  geom_line(size = 1, colour= "blue") +  # Line plot
  labs(title = "",
       x = "Date", 
       y = "Rain (mm)") +
  theme_minimal(base_size = 16) +  # Adjust base size for readability
  theme(
    axis.title.x = element_text(size = 22, face = "bold"),  # Larger, bold x-axis title
    axis.title.y = element_text(size = 22, face = "bold"),  # Larger, bold y-axis title
    axis.text.x = element_text(size = 16, angle = 0, hjust = 0.5),  # Horizontal x-axis text
    axis.text.y = element_text(size = 16),  # Size 16 for y-axis text
    plot.title = element_text(size = 24, face = "bold"),  # Larger, bold title
    legend.title = element_text(size = 18),  # Legend title size
    legend.text = element_text(size = 16),  # Legend text size
    panel.spacing = unit(1, "lines")  # Space between elements if needed
  )

# Convert ggplot to an interactive plotly plot
interactive_rain_plot <- ggplotly(gg_rain)

# Display the interactive plot
interactive_rain_plot

# Save the ggplot as a PNG file with high resolution
ggsave("rain_plot.png", plot = gg_rain, width = 10, height = 6, dpi = 300)
#########################################################################################################################################################
site_colors <- viridis::viridis(1)  # Use just one color for Trib5

# Plot: Temperature for Trib5 without any highlight
gg_temp_trib5 <- ggplot(subset(data_long, site == "Trib5_T1_Avg"), aes(x = timestamp, y = Temperature, color = site, 
                                                                       text = paste("Site:", site))) +
  geom_line(size = 0.5) +  # Line plot for temperature
  scale_color_manual(values = c("Trib5_T1_Avg" = site_colors)) +  # Custom color for Trib5
  labs(title = "",
       x = "Date", 
       y = "Temperature (°C)") +
  theme_minimal(base_size = 16) +  # Base size for overall readability
  theme(
    axis.title.x = element_text(size = 22, face = "bold"),  # Larger, bold x-axis title
    axis.title.y = element_text(size = 22, face = "bold"),  # Larger, bold y-axis title
    axis.text.x = element_text(size = 16, angle = 0, hjust = 0.5),  # Horizontal x-axis text
    axis.text.y = element_text(size = 16),  # Size 16 for y-axis text
    plot.title = element_text(size = 24, face = "bold"),  # Larger, bold title
    legend.title = element_text(size = 18),  # Legend title size
    legend.text = element_text(size = 16),  # Legend text size
    panel.spacing = unit(1, "lines")  # Space between elements if needed
  )

# Convert ggplot to an interactive plotly plot
interactive_temp_trib5_plot <- ggplotly(gg_temp_trib5, tooltip = "text")

# Display the interactive plot
interactive_temp_trib5_plot

ggsave("trib5_plot.png", plot = gg_temp_trib5, width = 10, height = 6, dpi = 300)

###########################################################################################################################################################
#all tribs no highlight
# Generate color-blind friendly palette using viridis for the 5 sites
site_colors <- viridis::viridis(6)  # Create a distinct color palette for the 6 sites

# Plot: Temperature time series for all sites without any highlight
gg_temp <- ggplot(data_long, aes(x = timestamp, y = Temperature, color = site, 
                                 text = paste("Site:", site))) +
  geom_line(size = 0.5) +  # Line plot for temperature
  scale_color_manual(values = setNames(site_colors, 
                                       c("Trib5_T1_Avg", "Trib4_T1_Avg", "Trib3_T1_Avg", "Trib2_T1_Avg", "Trib1_T1_Avg", "Trib6_T1_Avg"))) +
  labs(title = "",
       x = "Date", 
       y = "Temperature (°C)") +
  theme_minimal(base_size = 16) +  # Base size for overall readability
  theme(
    axis.title.x = element_text(size = 22, face = "bold"),  # Larger, bold x-axis title
    axis.title.y = element_text(size = 22, face = "bold"),  # Larger, bold y-axis title
    axis.text.x = element_text(size = 16, angle = 0, hjust = 0.5),  # Horizontal x-axis text
    axis.text.y = element_text(size = 16),  # Size 16 for y-axis text
    plot.title = element_text(size = 24, face = "bold"),  # Larger, bold title
    legend.title = element_text(size = 18),  # Legend title size
    legend.text = element_text(size = 16),  # Legend text size
    panel.spacing = unit(1, "lines")  # Space between elements if needed
  )

# Convert ggplot to an interactive plotly plot
interactive_temp_plot <- ggplotly(gg_temp, tooltip = "text")

# Display the interactive plot
interactive_temp_plot

ggsave("trib_plot.png", plot = gg_temp, width = 10, height = 6, dpi = 300)


library(ggplot2)
library(plotly)
#######################################################################################################################################################
###daily temp all tribs graph labels dont work
# Plot 1: Temperature with highlighted portion for one site
gg_temp <- ggplot(data_long_daily, aes(x = day, y = Temperature, color = site, 
                                       text = paste("Site:", site, 
                                                    "<br>Date:", format(day, "%Y-%m-%d"),
                                                    "<br>Temperature:", round(Temperature, 2), "°C"))) +
  geom_line(size = 0.5) +  # Line plot for temperature
  labs(title = "",
       x = "Date", 
       y = "Temperature (°C)") +
  theme_minimal(base_size = 16) +  # Base size for overall readability
  theme(
    axis.title.x = element_text(size = 22, face = "bold"),  # Larger, bold x-axis title
    axis.title.y = element_text(size = 22, face = "bold"),  # Larger, bold y-axis title
    axis.text.x = element_text(size = 16, angle = 45, hjust = 1),  # Adjust x-axis text angle for better readability
    axis.text.y = element_text(size = 16),  # Size 16 for y-axis text
    plot.title = element_text(size = 24, face = "bold"),  # Larger, bold title
    legend.title = element_text(size = 18),  # Legend title size
    legend.text = element_text(size = 16),  # Legend text size
    panel.spacing = unit(1, "lines")  # Space between elements if needed
  )

# Convert ggplot to plotly for interactivity
gg_temp_plotly <- ggplotly(gg_temp, tooltip = "text")

# Display the plot
gg_temp_plotly

#######################################################################################################################################################
#all tribs no highlight -- labels work
##makes labels show correctly -- no highlight
# Simplified ggplot plot to verify data and rendering
gg_temp <- ggplot(data_long, aes(x = timestamp, y = Temperature, color = site)) +
  geom_line(size = 0.5) +  # Line plot for temperature
  labs(title = "",
       x = "Date", 
       y = "Temperature (°C)") +
  theme_minimal(base_size = 16) +  # Base size for overall readability
  theme(
    axis.title.x = element_text(size = 22, face = "bold"),  # Larger, bold x-axis title
    axis.title.y = element_text(size = 22, face = "bold"),  # Larger, bold y-axis title
    axis.text.x = element_text(size = 16, angle = 45, hjust = 1),  # Adjust x-axis text angle for better readability
    axis.text.y = element_text(size = 16),  # Size 16 for y-axis text
    plot.title = element_text(size = 24, face = "bold"),  # Larger, bold title
    legend.title = element_text(size = 18),  # Legend title size
    legend.text = element_text(size = 16),  # Legend text size
    panel.spacing = unit(1, "lines")  # Space between elements if needed
  )

# Display simplified ggplot plot
gg_temp

library(plotly)

# Convert the simplified ggplot to plotly
gg_temp_plotly <- ggplotly(gg_temp, tooltip = c("x", "y", "text"))

# Display the plotly plot
gg_temp_plotly
#########################################################################################################################################################
# Calculate daily diurnal range for Trib 5
df_trib5 <- df %>%
  mutate(Date = as.Date(Date)) %>%
  group_by(Date) %>%
  summarise(Trib5 = max(range_SalmTrib5_T1_Avg, na.rm = TRUE) - min(range_SalmTrib5_T1_Avg, na.rm = TRUE)) 

# Compute IQR-based threshold for high diurnal ranges
stats <- df %>%
  summarise(
    Q1 = quantile(range_SalmTrib5_T1_Avg, 0.25, na.rm = TRUE),
    Q3 = quantile(range_SalmTrib5_T1_Avg, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1
  ) %>%
  mutate(UpperThreshold = Q3 + 1.5 * IQR)  # Flagging values above this as potential dewatering

# Check the calculated stats (Q1, Q3, IQR, and UpperThreshold)
print(stats)
# Add the UpperThreshold value to the df_trib5 dataset
df_trib5 <- df %>%
  mutate(Dewatered = range_SalmTrib5_T1_Avg > pull(stats, UpperThreshold))  # Use pull to extract the threshold value

# View flagged dewatered days
df_dewatered <- df_trib5 %>% filter(Dewatered)
print(df_dewatered)



