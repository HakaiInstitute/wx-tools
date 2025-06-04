###WSC dataframes
# Install and load necessary packages
if (!requireNamespace("tidyhydat", quietly = TRUE)) {
  install.packages("tidyhydat")
}
library(tidyhydat)
library(dplyr)

download_hydat()

#08HF015 Eve below Kunnum HGH
#08HD007 Salmon above Memekay HGH
#08HD006 Salmon near Sayward HGH TW
#08HF004 Tsitika below Catharine HGH TW


start_timestamp <- as.POSIXct("2024-05-03 11:00:00", tz = "UTC")

nc50_wsc <-realtime_ws(
  station_number = c("08HD006","08HD007","08HF015","08HF004"),
  parameters = c(46, 5), ## see param_id for a list of codes 5 - TW, 46 - HGH
  start_date = start_timestamp, # number of days before today
  end_date = Sys.Date()
)

library(plotly)

library(plotly)
library(dplyr)

# Mapping station IDs to titles
station_titles <- list(
  "08HF015" = "Eve below Kunnum",
  "08HD007" = "Salmon above Memekay",
  "08HD006" = "Salmon near Sayward",
  "08HF004" = "Tsitika below Catharine"
)

# Filter data for Parameters "5" and "46"
parameter_5_data <- nc50_wsc %>%
  filter(Parameter == "5")

parameter_46_data <- nc50_wsc %>%
  filter(Parameter == "46")

# Unique Station IDs
station_ids <- unique(nc50_wsc$STATION_NUMBER)

# Generate figures per station ID if data is available for each parameter
plots <- lapply(station_ids, function(station_id) {
  
  # Initialize list to store plots for the current station
  station_plots <- list()
  
  # Get station title if available
  station_title <- station_titles[[station_id]]
  
  # Filter data for each station ID and Parameter 5
  station_data_5 <- parameter_5_data %>%
    filter(STATION_NUMBER == station_id)
  
  if (nrow(station_data_5) > 0) {  # Only plot if data exists for Parameter 5
    fig1 <- plot_ly(data = station_data_5, x = ~Date, y = ~Value, type = 'scatter', mode = 'lines',
                    line = list(color = 'blue'), name = paste(station_id, "Water Level (m)")) %>%
      layout(
        title = list(text = paste("Water Level - Station", station_id, "-", station_title)),
        yaxis = list(title = "Temperature (°C)"),
        xaxis = list(title = "Date")
      )
    station_plots$fig1 <- fig1
  }
  
  # Filter data for each station ID and Parameter 46
  station_data_46 <- parameter_46_data %>%
    filter(STATION_NUMBER == station_id)
  
  if (nrow(station_data_46) > 0) {  # Only plot if data exists for Parameter 46
    fig2 <- plot_ly(data = station_data_46, x = ~Date, y = ~Value, type = 'scatter', mode = 'lines',
                    line = list(color = 'red'), name = paste(station_id, "Water Temperature (°C)")) %>%
      layout(
        title = list(text = paste("Water Temperature - Station", station_id, "-", station_title)),
        yaxis = list(title = "Temperature (°C)"),
        xaxis = list(title = "Date")
      )
    station_plots$fig2 <- fig2
  }
  
  # Only return plots if at least one plot was created
  if (length(station_plots) > 0) {
    return(station_plots)
  } else {
    return(NULL)  # Return NULL if no plots were created for this station
  }
})

# Filter out any NULL elements to keep only stations with generated plots
plots <- Filter(Negate(is.null), plots)

# Display each plot by accessing plots[[station_id]]$fig1 and plots[[station_id]]$fig2
plots
