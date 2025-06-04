library(dplyr)
library(tidyr)

# Convert timestamp column to POSIXct
df$timestamp <- as.POSIXct(df$timestamp, format="%Y-%m-%d %H:%M", tz="UTC")
df = select(df, -c(8))

# Example: Assume df has a DateTime column and each other column represents a stream
df <- df %>%
  mutate(Date = as.Date(timestamp)) %>%  # Extract date
  group_by(Date) %>% 
  summarise(across(-timestamp, ~ max(.x, na.rm = TRUE) - min(.x, na.rm = TRUE), .names = "range_{.col}")) %>%
  ungroup()

# Calculate average daily range for each stream
avg_diurnal_range <- df %>%
  summarise(across(starts_with("range_"), \(x) mean(x, na.rm = TRUE)))

print(df)  # Daily range for each stream
print(avg_diurnal_range)  # Average diurnal fluctuation per stream


# Convert data from wide to long format
df_long <- df %>%
  pivot_longer(cols = starts_with("range_"), names_to = "Stream", values_to = "Range") %>%
  mutate(Stream = gsub("range_", "", Stream))  # Remove prefix for cleaner labels

# Compute IQR and identify outliers
df_outliers <- df_long %>%
  group_by(Stream) %>%
  mutate(Q1 = quantile(Range, 0.25, na.rm = TRUE),
         Q3 = quantile(Range, 0.75, na.rm = TRUE),
         IQR = Q3 - Q1,
         Lower = Q1 - 1.5 * IQR,
         Upper = Q3 + 1.5 * IQR,
         Outlier = Range < Lower | Range > Upper) %>%
  ungroup()


# Plot daily range over time for each stream
ts<-ggplot(df_long, aes(x = Date, y = Range, color = Stream)) +
  geom_line(size = 1) +
  labs(title = "Daily Temperature Range by Stream",
       x = "Date",
       y = "Temperature Range (°C)",
       color = "Stream") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 36, face = "bold", hjust = 0.5),  # Title size and alignment
    axis.title.x = element_text(size = 28, face = "bold"),  # X-axis title size
    axis.title.y = element_text(size = 28, face = "bold"),  # Y-axis title size
    axis.text.x = element_text(size = 22),  # X-axis text size
    axis.text.y = element_text(size = 22),  # Y-axis text size
    legend.title = element_text(size = 24, face = "bold"),  # Legend title size
    legend.text = element_text(size = 22),  # Legend text size
    panel.grid.major = element_blank(),  # Remove major grid lines for clarity
    panel.grid.minor = element_blank()   # Remove minor grid lines for clarity
  )
ts
#boxplot
bs<-ggplot(df_long, aes(x = Stream, y = Range, fill = Stream)) +
  geom_boxplot() +
  labs(title = "Distribution of Diurnal Temperature Ranges",
       x = "Stream",
       y = "Temperature Range (°C)") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 22, angle = 45, hjust = 1, vjust = 1),  # Rotate x-axis labels
    plot.title = element_text(size = 36, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 28, face = "bold"),
    axis.title.y = element_text(size = 28, face = "bold"),
    axis.text.y = element_text(size = 22),
    legend.title = element_text(size = 24, face = "bold"),
    legend.text = element_text(size = 22),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )+
  theme(legend.position = "none")

#outliers highlighted
ggplot(df_outliers, aes(x = Stream, y = Range, fill = Stream)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.5) +  # Hide default outliers
  geom_jitter(data = filter(df_outliers, Outlier), aes(color = Stream), size = 2, width = 0.2) +  # Highlight outliers
  labs(title = "Distribution of Diurnal Temperature Ranges with Outliers",
       x = "Stream",
       y = "Temperature Range (°C)") +
  theme_minimal() +
  theme(legend.position = "none")

#heatmap
hm<-ggplot(df_long, aes(x = Date, y = Stream, fill = Range)) +
  geom_tile() +
  scale_fill_viridis_c() +  # Use a color gradient for better visibility
  labs(title = "Heatmap of Daily Temperature Range",
       x = "Date",
       y = "Stream",
       fill = "Temp Range (°C)") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 36, face = "bold", hjust = 0.5),  # Title size and alignment
    axis.title.x = element_text(size = 28, face = "bold"),  # X-axis title size
    axis.title.y = element_text(size = 28, face = "bold"),  # Y-axis title size
    axis.text.x = element_text(size = 22),  # X-axis text size
    axis.text.y = element_text(size = 22),  # Y-axis text size
    legend.title = element_text(size = 24, face = "bold"),  # Legend title size
    legend.text = element_text(size = 22),  # Legend text size
    panel.grid.major = element_blank(),  # Remove major grid lines for clarity
    panel.grid.minor = element_blank()   # Remove minor grid lines for clarity
  )

df_anomalies <- df_outliers %>% filter(Outlier)
print(df_anomalies)

ggsave("trib5_ts.png", ts, width = 16, height = 9, dpi = 300, units = "in")
ggsave("trib5_bp.png", bs, width = 16, height = 9, dpi = 300, units = "in")
ggsave("trib5hm.png", hm, width = 16, height = 9, dpi = 300, units = "in")




#################################################################
#rough dewater detection code

library(dplyr)
library(ggplot2)

