# Load necessary libraries
library(dplyr)
library(plotly)

# Function to generate a Plotly plot for a specific trib or outlet
create_plot <- function(data, pattern, title) {
  # Select the timestamp and relevant columns
  selected_data <- data %>%
    select(timestamp, contains(pattern))
  
  # Print available columns for debugging
  available_cols <- names(selected_data)
  message(paste("Available columns for pattern", pattern, ":", paste(available_cols, collapse = ", ")))
  
  # Identify temperature and YSI columns based on the pattern
  temp_col <- grep(paste0("^salm.*_", pattern), available_cols, value = TRUE)
  ysi_col <- grep(paste0("^ysi_temp_", pattern), available_cols, value = TRUE)
  
  # Check if columns exist
  if (length(temp_col) == 0 || length(ysi_col) == 0) {
    message(paste("Required columns for", pattern, "not found."))
    return(NULL)
  }
  
  # Create the Plotly plot
  plot <- plot_ly(selected_data, x = ~timestamp, y = ~.data[[temp_col]], type = 'scatter', mode = 'lines', name = "Water Temperature") %>%
    add_trace(y = ~.data[[ysi_col]], mode = 'markers', name = "YSI Points") %>%
    layout(
      title = title,
      xaxis = list(title = "Timestamp"),
      yaxis = list(title = "Temperature (°C)")
    )
  
  return(plot)
}

# Generate plots for each "trib" and outlet
trib2_plot <- create_plot(df, "trib2", "SALM_TRIB2 Temperature")
trib1_plot <- create_plot(df, "trib1", "SALM_TRIB1 Temperature")
trib6_plot <- create_plot(df, "trib6", "SALM_TRIB6 Temperature")
outlet1_plot <- create_plot(df, "outlet1", "SALM_OUTLET1 Temperature")

# Display the plots
trib2_plot
trib1_plot
trib6_plot
outlet1_plot


#outlet1
outlet1_data <- df %>%
  select(timestamp, contains("outlet1"))

# Create the Plotly plot for trib6 with ysi  temperature points trib1
outlet_plot <- plot_ly(outlet1_data, x = ~timestamp, y = ~salm_outlet1_t1_21729340, type = 'scatter', mode = 'lines', name = "Tidbit") %>%
  add_trace(y = ~ysi_temp_salm_outlet1, mode = 'markers', name = "YSI") %>%
  layout(
    title = "SALM_OUTLET1 Temperature",
    xaxis = list(title = "Timestamp"),
    yaxis = list(title = "Temperature (°C)")
  )
outlet_plot
