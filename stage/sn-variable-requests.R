# Define the sites and variable patterns
# sites <- c("Buxton",
#            "BuxtonEast",
#            "Ethel",
#            "Hecate",
#            "Koeye",
#            "Lookout",
#            "PruthDock",
#            "Quadra",
#            "RefStn",
#            "SSN1015US",
#            "SSN626PWR",
#            "SSN693PWR",
#            "SSN708",
#            "SSN819PWR",
#           "WSN626",
#            "WSN693_703",
#            "WSN703",
#            "WSN703_708",
#            "WSN819_1015",
#            "WSN844")

sites <- c("SSN1015US",
           "SSN626US",
           "SSN703US",
           "SSN844US")

components <- c("5minuteSamples.PLS_Lvl", 
                "5minuteSamples.PLS_Temp")

# Generate and sort the variable names
variable_names <- expand.grid(Site = sites, Component = components) %>%
  arrange(Site) %>% # Ensure sorting by Site
  mutate(Variable = paste(Site, Component, sep = ".")) %>%
  pull(Variable)

# Print the sorted variable names
print(variable_names)

write.csv(variable_names, "variable_names.csv")

file_path<-"C:/Users/Emily/Downloads/sensor-network/2024-12-01-process-sn-data-manual-QC.log"
logs<-readLines(file_path)

# Extract site names
site_names <- logs %>%
  str_extract("\\[.*?\\]") %>% # Extract everything inside square brackets
  str_remove_all("[\\[\\]]") %>% # Remove the square brackets
  str_extract("^[^\\.]+") %>% # Extract the part before the first dot
  unique() # Get unique site names

# Print the unique site names
print(site_names)
write.csv(site_names,"sensor_network_names.csv")
