#read in data

library(readr)
library(plotly)

df <- read_csv("data/external-wx/pcds_data/FLNRO-WMB/FLNRO_WMB_Menzies.csv", 
               col_types = cols(timestamp = col_datetime(format = "%Y-%m-%d %H:%M:%S")))
View(df)

# Plot for air temperature
air_temp_plot <- plot_ly(df, x = ~timestamp, y = ~air_temp, type = 'scatter', mode = 'lines', 
                         line = list(color = 'red'), name = "Air Temperature (°C)") %>%
  layout(xaxis = list(title = 'Timestamp'),
         yaxis = list(title = 'Air Temperature (°C)'))

# Plot for precipitation
rain_plot <- plot_ly(df, x = ~timestamp, y = ~precipitation, type = 'scatter', mode = 'lines', 
                     line = list(color = 'green'), name = "Precipitation (mm)") %>%
  layout(xaxis = list(title = 'Timestamp'),
         yaxis = list(title = 'Precipitation (mm)'))

# Combine both plots in a subplot
subplot(air_temp_plot, rain_plot, nrows = 2, shareX = TRUE) %>% 
  layout(title="NC50 Salmon Station [630 masl]")
rain_plot



#mwat data
filtered_df <- model_data %>%
  filter(grepl("SALM", station)) %>%  # Filter rows containing "SALM" in the 'station' column
  select(mwat, mwat.date)


# Define custom color values for stations (same as before)
station_colors <- c(
  "salm_trib9_t1" = "#1f77b4",  
  "salm_trib8_t1" = "#2ca02c",  
  "salm_trib7_t1" = "#d62728",  
  "salm_trib6_t1" = "#ff7f0e",  
  "salm_trib5_t1" = "#9467bd",  
  "salm_trib4_t1" = "#e377c2",  
  "salm_trib3_t1" = "#8c564b",  
  "salm_trib2_t1" = "#7f7f7f",  
  "salm_trib20bt1" = "#bcbd22", 
  "salm_trib1_t1" = "#17becf",  
  "salm_trib19_t1" = "#f0e3f0", 
  "salm_trib17_t1" = "#98df8a", 
  "salm_trib14_t2" = "#c5b0d5", 
  "salm_trib11_t1" = "#ff9896", 
  "salm_trib10_t1" = "#9467bd",  
  "salm_outlet1_t2" = "#c7c7c7",
  "SALM_TRIB10_T1" = "#bcbd22", 
  "SALM_TRIB11_T1" = "#d62728", 
  "SALM_TRIB19_T1" = "#ff7f0e", 
  "SALM_TRIB6_T1" = "#2ca02c", 
  "SALM_TRIB17_T1" = "#9467bd", 
  "SALM_TRIB7_T1" = "#8c564b",  
  "SALM_TRIB8_T1" = "#e377c2",  
  "SALM_TRIB9_T1" = "#1f77b4"
)

# Define custom legend labels for stations (same as before)
custom_legend_labels <- c(
  "salm_trib9_t1" = "Salmon Trib 9", 
  "salm_trib8_t1" = "Salmon Trib 8", 
  "salm_trib7_t1" = "Salmon Trib 7", 
  "salm_trib6_t1" = "Salmon Trib 6", 
  "salm_trib5_t1" = "Salmon Trib 5", 
  "salm_trib4_t1" = "Salmon Trib 4", 
  "salm_trib3_t1" = "Salmon Trib 3", 
  "salm_trib2_t1" = "Salmon Trib 2", 
  "salm_trib20bt1" = "Salmon Trib 20b", 
  "salm_trib1_t1" = "Salmon Trib 1", 
  "salm_trib19_t1" = "Salmon Trib 19", 
  "salm_trib17_t1" = "Salmon Trib 17", 
  "salm_trib14_t2" = "Salmon Trib 14", 
  "salm_trib11_t1" = "Salmon Trib 11", 
  "salm_trib10_t1" = "Salmon Trib 10", 
  "salm_outlet1_t2" = "Salmon Outlet 1")

# Convert both the `station` column and `custom_legend_labels` to lowercase
filtered_df$station <- str_to_lower(filtered_df$station)
station_colors <- setNames(station_colors, str_to_lower(names(station_colors)))
custom_legend_labels <- setNames(custom_legend_labels, str_to_lower(names(custom_legend_labels)))


# Plot the main data as line plot
ggplot_fig <- ggplot(data_long, aes(x = timestamp, y = Temperature, color = Site)) +
  geom_line() +
  labs(
    title = "Monitoring Sites- Salmon Watershed",
    x = "Date",
    y = "Temperature (°C)"
  ) +
  theme_minimal()

# Add the additional points as colored dots with black outline
ggplot_fig <- ggplot_fig +
  geom_point(data = filtered_df, aes(x = mwat.date, y = mwat, color = station), 
             size = 3, shape = 21, stroke = 1, fill = "black") +  # Fill is white, outline is colored by color
  scale_color_manual(values = station_colors, labels = custom_legend_labels) +  # Apply custom colors and labels
  scale_fill_manual(values = station_colors) +  # Fill the interior of the points with the same color
  guides(color = guide_legend(title = "Station", labels = custom_legend_labels), 
         fill = "none")  # Remove fill from legend

# Convert to interactive plotly plot
interactive_plot_mwat <- ggplotly(ggplot_fig)

# Show the interactive plot
interactive_plot_mwat