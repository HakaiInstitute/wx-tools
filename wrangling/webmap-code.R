# Install and load necessary packages
install.packages("leaflet")
library(leaflet)


# Your data
data <- data.frame(
  station = c("LOOKOUT", "refstn", "pruth dock"),
  lat = c(51.6475, 51.65195, 51.65455),
  long = c(-128.143183, -128.1287, -128.129426),
  masl = c(63, 43, 5),
  temp = c(2, 2, 2),
  rain = c(1, 1, 1),
  precip = c(NA, 1, NA),
  sr50 = c(1, 1, 1),
  radiation = c(1, 1, 1),
  rh = c(1, 1, 1),
  wind = c(1, 1, NA),
  water_depth = c(NA, NA, NA),
  EC = c(NA, NA, NA),
  turbidity = c(NA, NA, NA),
  hobo_water_depth = c(NA, NA, NA),
  hobo_temp = c(NA, NA, NA),
  networked = c("Y", "Y", "Y"),
  total = c(7, 6, 20)
)

# Create a Leaflet map with popups
map<-leaflet(data) %>%
  addTiles() %>%
  addCircleMarkers(
    ~long, ~lat,
    radius = ~sqrt(total)*5,
    color = "blue",
    fillOpacity = 0.8,
    popup = ~paste("<b>Station:</b> ", station,
                            "<br><b>Latitude:</b> ", lat,
                            "<br><b>Longitude:</b> ", long,
                            "<br><b>MASL:</b> ", masl,
                            "<br><b>Temperature:</b> ", temp,
                            "<br><b>Rain:</b> ", rain,
                            "<br><b>Precipitation:</b> ", precip,
                            "<br><b>SR50:</b> ", sr50,
                            "<br><b>Radiation:</b> ", radiation,
                            "<br><b>Relative Humidity:</b> ", rh,
                            "<br><b>Wind:</b> ", wind,
                            "<br><b>water_depth:</b> ", water_depth,
                            "<br><b>EC:</b> ", EC,
                            "<br><b>Turbidity:</b> ", turbidity,
                            "<br><b>SA_watlev:</b> ", SA_watlev,
                            "<br><b>sa_wattemp:</b> ", sa_wattemp,
                            "<br><b>Networked:</b> ", networked)) %>%
  addProviderTiles("Esri.WorldImagery", options = providerTileOptions(noWrap = TRUE))

map

saveWidget(map, file = "sensor-inventory-map.html")



leaflet(data) %>%
  addTiles() %>%
  addCircleMarkers(
    ~long, ~lat,
    radius = ~sqrt(total) * 5,  # Scale the bubble size based on the 'total' column
    color = "blue",
    fillOpacity = 0.8,
    popup = ~paste("<b>Station:</b> ", station,
                   "<br><b>Total:</b> ", total)
  ) %>%
  addProviderTiles("Esri.WorldImagery", options = providerTileOptions(noWrap = TRUE))
