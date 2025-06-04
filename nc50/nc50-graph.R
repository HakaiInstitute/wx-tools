library(ggplot2)
library(plotly)
library(dplyr)
library(readr)
library(webshot2)
library(htmlwidgets)
library(RColorBrewer)

# Define the output directory and prefix
output_dir <- "C:/Users/Emily/Documents/git-repos/wx-tools/NC50-data/output_figs"
file_prefix <- "ADAM" # Replace with your 4-letter file prefix

# List all CSV files starting with the specified prefix
file_list <- list.files(pattern = paste0("^", file_prefix, ".*\\.csv$"))

# Read and combine data from all files
combined_data <- file_list %>%
  lapply(function(file) {
    data <- read_csv(file)
    # Ensure correct column names
    colnames(data) <- c("timestamp", "temperature")
    data %>%
      mutate(filename = file) # Add filename column to track data source
  }) %>%
  bind_rows()

# Create the line plot
plot <- ggplot(combined_data, aes(x = timestamp, y = temperature, color = filename)) +
  geom_line() +
  labs(title = paste("Time Series Plot for", file_prefix),
       x = "Timestamp",
       y = "Temperature") +
  theme_minimal() +
  scale_color_discrete(name = "File")

# Convert the plot to an interactive plotly object
interactive_plot <- ggplotly(plot)

# Define the HTML file name
html_filename <- paste0(output_dir, file_prefix, "_time_series.html")

# Save the plot as an HTML file
saveWidget(interactive_plot, file = html_filename, selfcontained = TRUE)

# Define the PNG file name
png_filename <- paste0(output_dir, file_prefix, "_time_series.png")

# Capture the HTML file as a PNG image
webshot(html_filename, file = png_filename, vwidth = 1000, vheight = 600)

# Print the filename to confirm
print(paste("Plot saved as:", png_filename))

#####################################################################################################################################
#for nc50 workshop 2025-01-22
###################################################################################################################################

# Define folder path
folder_path <- "C:/Users/Emily/Documents/git-repos/wx-tools/NC50-data-qc/data-output"


# Get the list of files matching the patterns
file_patterns <- c("heyd", "lull", "read", "fulm", "tuna")
files <- list.files(folder_path, pattern = paste(file_patterns, collapse = "|"), full.names = TRUE)

library(ggplot2)
library(lubridate)
library(plotly)

# Define folder path
folder_path <- "path_to_your_folder"

# Get the list of files matching the patterns
file_patterns <- c("heyd", "lull", "read", "fulm", "tuna")
files <- list.files(folder_path, pattern = paste(file_patterns, collapse = "|"), full.names = TRUE)

# Loop through each file to create and save plots
for (file in files) {
  # Read the data
  data <- read.csv(file)
  
  # Ensure the file has at least 3 columns
  if (ncol(data) < 3) {
    message(paste("Skipping file due to insufficient columns:", file))
    next
  }
  
  # Rename columns for clarity
  colnames(data)[1:3] <- c("timestamp", "temperature", "qc_flag")
  
  # Convert timestamp column to POSIXct
  data$timestamp <- as.POSIXct(data$timestamp, format = "%Y-%m-%d %H:%M:%S")
  
  # Calculate 10 days after the minimum timestamp
  min_date <- min(data$timestamp, na.rm = TRUE)
  ten_days_after_min <- min_date + days(10)
  
  # Add a new column for line color based on QC flag
  data$line_color <- ifelse(grepl("AV", data$qc_flag, ignore.case = TRUE), "blue", "red")
  
  # Create the ggplot
  p <- ggplot(data, aes(x = timestamp, y = temperature, group = 1)) +
    geom_line(aes(color = line_color), linewidth = 0.5, show.legend = FALSE) + # Line with conditional color
    scale_color_identity() +  # Use the actual colors defined in the "line_color" column
    geom_hline(yintercept = 19, linetype = "dashed", color = "black") +
    geom_hline(yintercept = 21, linetype = "dashed", color = "black") +
    # Adjust the horizontal and vertical positioning of the 19°C label
    annotate("text", x = ten_days_after_min, y = 19.5, 
             label = "19°C Threshold", 
             hjust = 0, vjust = 0, color = "black", size = 3.5, fontface = "bold") +
    # Adjust the horizontal and vertical positioning of the 21°C label
    annotate("text", x = ten_days_after_min, y = 21.5, 
             label = "21°C Threshold", 
             hjust = 0, vjust = 0, color = "black", size = 3.5, fontface = "bold") +
    labs(x = "Date",
         y = "Temperature (°C)") +
    theme_minimal()
  
  # Convert ggplot to plotly for interactivity
  plotly_plot <- ggplotly(p)
  
  # Save the plot as an HTML file
  output_file <- file.path(folder_path, paste0(tools::file_path_sans_ext(basename(file)), "_plot.html"))
  htmlwidgets::saveWidget(as_widget(plotly_plot), output_file)
  
  message(paste("Saved plot to:", output_file))
}
#################################################################################################
#ggplot

# Loop through each file to create and save plots
for (file in files) {
  # Read the data
  data <- read.csv(file)
  
  # Ensure the file has at least 3 columns
  if (ncol(data) < 3) {
    message(paste("Skipping file due to insufficient columns:", file))
    next
  }
  
  # Rename columns for clarity
  colnames(data)[1:3] <- c("timestamp", "temperature", "qc_flag")
  
  # Convert timestamp column to POSIXct
  data$timestamp <- as.POSIXct(data$timestamp, format = "%Y-%m-%d %H:%M:%S")
  
  # Calculate 10 days after the minimum timestamp
  min_date <- min(data$timestamp, na.rm = TRUE)
  ten_days_after_min <- min_date + days(10)
  
  # Add a new column for line color based on QC flag
  data$line_color <- ifelse(grepl("AV", data$qc_flag, ignore.case = TRUE), "blue", "red")
  
  # Choose a colorblind-friendly palette (from RColorBrewer)
  color_palette <- brewer.pal(3, "Set1")  # Using the 'Set1' palette with 3 colors (you can adjust if necessary)
  
  # Create the ggplot
  p <- ggplot(data, aes(x = timestamp, y = temperature, group = 1)) +
    geom_line(aes(color = line_color), linewidth = 0.5, show.legend = FALSE) + # Line with conditional color
    scale_color_manual(values = c("red" = color_palette[1], "blue" = color_palette[2])) +  # Apply color palette
    geom_hline(yintercept = 19, linetype = "dashed", color = "orange") +
    geom_hline(yintercept = 21, linetype = "dashed", color = "red") +
    # Adjust the horizontal and vertical positioning of the 19°C label
    annotate("text", x = ten_days_after_min, y = 19.5, 
             label = " ", 
             hjust = 0, vjust = 0, color = "orange", size = 3.5, fontface = "bold") +
    # Adjust the horizontal and vertical positioning of the 21°C label
    annotate("text", x = ten_days_after_min, y = 21.5, 
             label = " ", 
             hjust = 0, vjust = 0, color = "red", size = 3.5, fontface = "bold") +
    labs(x = "Date",
         y = " ") +
    theme_minimal()
  
  # Save the plot as a PNG file (or another format if preferred)
  output_file <- file.path(folder_path, paste0(tools::file_path_sans_ext(basename(file)), "_plot.png"))
  ggsave(output_file, plot = p, width = 10, height = 6)
  
  message(paste("Saved plot to:", output_file))
}
##############################################################################################################
# Heydon example
# Plot temperature and rainfall on the same graph
pheyd <- ggplot(df) +
  # Temperature line
  geom_line(aes(x = date, y = temperature, color = "Temperature"), size = 1) +
  # Rainfall line
  geom_line(aes(x = date, y = rain, color = "Rainfall"), size = 1) +
  # Customize labels and colors
  scale_color_manual(values = c("Temperature" = "blue", "Rainfall" = "green")) +
  labs(
    x = "Date",
    y = "Value",
    title = "Heydon Weather Comparison",
    color = ""
  ) +
  theme_minimal()

# Display the plot
print(pheyd)
