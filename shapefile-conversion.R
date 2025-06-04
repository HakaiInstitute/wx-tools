
#set working directory
setwd("/YOUR/FILE/LOCATIONS")

# check your working directory is in the right spot
getwd()

# Install and load the necessary packages
install.packages("sf")
library(sf)

# Read the CSV file
csv_data <- read.csv("kwak-station-coords.csv")

# Create a simple feature (sf) object
sf_object <- st_as_sf(df, coords = c("long", "lat"), crs = 4326)

# Transform the sf object to British Columbia Albers (EPSG:3005)
sf_object_bc <- st_transform(sf_object, crs = 3005)

# Save the transformed sf object as a shapefile
st_write(sf_object, "output_shapefile_bc.shp")
