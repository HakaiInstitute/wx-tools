library(ggplot2)
library(dplyr)
library(viridis)  # Colorblind-friendly palette

# Define steps and sizes
steps <- data.frame(
  step = factor(c("1. Planning & Preparation", "2. Field Deployment & Data Collection", 
                  "3. Data Processing & Quality Control", "4. Analysis & Interpretation", 
                  "5. Reporting & Communication"), levels = rev(c("1. Planning & Preparation", 
                                                                  "2. Field Deployment & Data Collection", "3. Data Processing & Quality Control", 
                                                                  "4. Analysis & Interpretation", "5. Reporting & Communication"))),
  size = c(5, 4, 3, 2, 1) # Defines the relative width of each step
)

# Create an upside-down triangle chart
ggplot(steps, aes(x = size, y = step, fill = step)) +
  geom_col(width = 1, show.legend = FALSE, color = "black") +  # Add black outline
  scale_x_reverse() +  # Flip so it gets narrower at the bottom
  scale_fill_viridis_d(option = "C") +  # Use a colorblind-friendly discrete palette
  theme_minimal() +
  labs(title = "", 
       x = "", y = "") +
  theme(axis.text.y = element_text(size = 12, face = "bold", hjust = 0),  # Left-align text
        axis.text.x = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0, face = "bold"))

# Save as PNG (High-Resolution)
ggsave("science_project_phases.png", plot, width = 16, height = 9, dpi = 300, units = "in")
