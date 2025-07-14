
# 1. Load and Filter OBIS Cetacean Data -----------------------------------

# Load the raw OBIS cetacean occurrence data
cetaceans_obis <- read_excel("Products/0_RawData/cetaceansobis_raw_13012025.xlsx")

# Remove records not identified at the species level
invalid_names <- c("Delphinidae", "Balaenoptera", "Globicephala", "Cetacea", 
                   "Mesoplodon", "Physeteroidea", "Kogia", "Hyperoodon", 
                   "Mysticeti", "Stenella", "Kogiidae", "Odontoceti", 
                   "Ziphiidae", "Balaenopteridae", "Cephalorhynchus", 
                   "Lagenorhynchus obscurus", "Eubalaena")

cetaceans_obis <- cetaceans_obis %>%
  filter(!scientificName %in% invalid_names)

# Standardize scientific names
name_corrections <- c("Delphinus" = "Delphinus delphis", 
                      "Sousa" = "Sousa teuszii",
                      "Lagenorhynchus" = "Lagenorhynchus acutus",
                      "Phocoenidae" = "Phocoena phocoena",
                      "Balaenoptera musculus brevicauda" = "Balaenoptera musculus")

cetaceans_obis <- cetaceans_obis %>%
  mutate(scientificName = recode(scientificName, !!!name_corrections))

# Remove records from datasets that are not useful for analysis
excluded_datasets <- c("Historical distribution of whales shown by logbook records 1785-1913", 
                       "PELACUS", 
                       "CODA cetacean sightings on primary platform of vessel surveys 2007", 
                       "CETUS: Cetacean monitoring surveys in the Eastern North Atlantic", 
                       "SCANS II cetacean sightings on primary platform of vessel surveys 2005", 
                       "Historical strandings of cetaceans on the Portuguese coast", 
                       "Cetacean occurrence off the west central Portugal coast from boat-based surveys 2007-2008", 
                       "Cetacean monitoring data collected along fixed transect using ferries as platforms in the Strait of Gibraltar, 2018, FLT Med Network", 
                       "PIROP Northwest Atlantic 1965-1992", 
                       "CODA cetacean sightings on tracker platform of vessel surveys 2007", 
                       "SCANS II cetacean sightings on tracker platform of vessel surveys 2005", 
                       "Allied Humpback Whale Catalogue, 1976 - 2003", 
                       "Alnitak-Alnilam Cetaceans and sea turtles surveys off Southern Spain",
                       "Tethys Research Institute shipboard survey cetacean sightings 1986-2012",
                       "Observatoire Pelagis - Reseau National Echouage (French stranding network) strandings 1934-2020",
                       "Observatoire Pelagis boat surveys 2003-2021",
                       "SCANS I cetacean sightings 1994",
                       "SCANS II cetacean sightings from aerial surveys 2005",
                       "ICES Biodiversity - Cetaceans (JCDP)",  
                       "European Seabirds At Sea (ESAS)",   
                       "EC-CWS Eastern Canada Seabirds at Sea")

cetaceans_obis <- cetaceans_obis %>%
  filter(!datasetName %in% excluded_datasets)

# Filter out future records
cetaceans_obis <- cetaceans_obis %>%
  filter(year < 2025)

# Add rounded latitude and longitude columns (lost when converting to sf)
cetaceans_obis$lat <- round(cetaceans_obis$decimalLatitude, 2)
cetaceans_obis$lon <- round(cetaceans_obis$decimalLongitude, 2)

# Convert to spatial object and crop to study area
observations_sfcet <- st_as_sf(cetaceans_obis, coords = c("decimalLongitude", "decimalLatitude"), crs = st_crs(shape))
cortecet <- st_intersection(observations_sfcet, shape)

# Fill in publisher field for consistency
cortecet$publisher <- cortecet$recordedBy

# 2. Save cleaned dataset -------------------------------------------------

write_xlsx(cortecet, path = "Products/1_L0/cetaceansobis_cleaned_13012025.xlsx")
cetaceans_obis <- read_excel("Products/1_L0/cetaceansobis_cleaned_13012025.xlsx")
