# Convert datetime column to POSIXct format if needed
dfo$Time <- as.POSIXct(dfo$Time)
str(dfo)


# Plotting
p<-ggplot(dfo, aes(x = Time)) +
  geom_line(aes(y = heydon_twtr, color = "Heydon River"), size = 1, linetype = "solid") +
  geom_line(aes(y = blackcreek_twtr, color = "Black Creek"), size = 1, linetype = "solid") +
  scale_color_manual(values = c("Heydon River" = "blue", "Black Creek" = "red")) +
  labs(x = "Date", y = "Water Temperature (°C)", color = "Station") +
  scale_x_datetime(date_labels = "%b %Y", date_breaks = "1 month") +
  theme_minimal() +
  theme(legend.position = "top")


ggsave("dfo-station-timeseries.png", plot = p, width = 10, height = 6, units = "in", dpi = 300)
