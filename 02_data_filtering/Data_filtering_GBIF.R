
# 1. Filter and Clean GBIF Cetacean Occurrence Data -----------------------

library(sf)
library(dplyr)
library(ggplot2)
library(readxl)
library(writexl)

# Load study area shapefile
study_area <- read_sf("./Study Area/PhD_studyarea/phd_studyarea.shp")

# Load raw data
cetaceans_gbif <- read_excel("Products/0_RawData/cetaceansgbif_raw_2025-01-13.xlsx")

# Remove records with unwanted basis of record
remove_basis <- c("PRESERVED_SPECIMEN", "FOSSIL_SPECIMEN", "MATERIAL_SAMPLE", 
                  "MATERIAL_CITATION", "LIVING_SPECIMEN")
cetaceans_gbif <- cetaceans_gbif %>% 
  filter(!basisOfRecord %in% remove_basis)

# Remove extinct families or records with missing family
remove_families <- c("Eschrichtiidae", "Platanistidae", "", "Lipotidae", NA)
cetaceans_gbif <- cetaceans_gbif %>% 
  filter(!family %in% remove_families)

# Standardize family names
cetaceans_gbif$family[cetaceans_gbif$family == "Hyperoodontidae"] <- "Ziphiidae"

# Round coordinates
cetaceans_gbif$lat <- round(cetaceans_gbif$decimalLatitude, 2)
cetaceans_gbif$lon <- round(cetaceans_gbif$decimalLongitude, 2)

# Convert to sf object and clip to study area
observations_sf <- st_as_sf(cetaceans_gbif, coords = c("decimalLongitude", "decimalLatitude"), crs = st_crs(study_area))
cetaceans_gbif <- st_intersection(observations_sf, study_area)

# Plot filtered data
ggplot() +
  geom_sf(data = study_area, fill = "white", color = "black") +
  geom_sf(data = cetaceans_gbif, aes(color = family), size = 2) +
  theme_minimal() +
  labs(title = "Cetacean Observations from GBIF",
       x = "Longitude", y = "Latitude")

# Clean and standardize species names
cetaceans_gbif$scientificName <- sub("^([A-Za-z]+\\s+[a-z]+).*", "\\1", cetaceans_gbif$scientificName)

cetaceans_gbif$scientificName <- ifelse(
  grepl("^[A-Za-z]+\\s+\\p{L}+[,\\s]*\\d{4}$", cetaceans_gbif$scientificName, perl = TRUE),
  sub("\\s+\\p{L}+.*$", "", cetaceans_gbif$scientificName, perl = TRUE),
  cetaceans_gbif$scientificName
)

# Remove generic or uncertain taxa
remove_species <- c("Phocoena spinipinnis", "Balaenoptera", "Delphinidae", "Globicephala", 
                    "Delphinus", "Mesoplodon", "Stenella", "Ziphiidae", "Kogiidae", 
                    "Balaenopteridae", "Hyperoodontidae", "Kogia", "Hyperoodon", 
                    "Cephalorhynchus", "Neophocaena phocaenoides", "Phocoenidae", 
                    "Orcaella brevirostris")
cetaceans_gbif <- cetaceans_gbif %>% 
  filter(!scientificName %in% remove_species)

# Correct outdated or incorrect names
name_corrections <- c("Tursiops truncantus" = "Tursiops truncatus",
                      "Sousa lentiginosa" = "Sousa teuszii",
                      "Physeter catodon" = "Physeter macrocephalus",
                      "Tursiops" = "Tursiops truncatus",
                      "Globicephala melaena" = "Globicephala melas",
                      "Delphinus capensis" = "Delphinus delphis",
                      "Lagenorhynchus" = "Lagenorhynchus acutus")
cetaceans_gbif <- cetaceans_gbif %>%
  mutate(scientificName = recode(scientificName, !!!name_corrections))

# Remove unreliable publishers
remove_publishers <- c("Administración de Parques Nacionales, Argentina", 
                       "Ministerio del Medio Ambiente de Chile", 
                       "National Museum of Nature and Science, Japan", 
                       "Questagame", 
                       "The National Institute of Water and Atmospheric Research (NIWA)", 
                       "Finnish Biodiversity Information Facility")
cetaceans_gbif <- cetaceans_gbif %>% 
  filter(!publisher %in% remove_publishers)

# Remove datasets not relevant to the study
remove_datasets <- c("Historical distribution of whales shown by logbook records 1785-1913", 
                     "PELACUS", "CODA cetacean sightings on primary platform of vessel surveys 2007", 
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
                     "Inforbiomares : Biodiversity Census", "InforBiomares",  
                     "Tethys Research Institute shipboard survey cetacean sightings 1986-2012", 
                     "Observatoire Pelagis boat surveys 2003-2021", 
                     "Observatoire Pelagis sightings from fishery surveys 2004-2009", 
                     "European Seabirds At Sea (ESAS)", 
                     "JNCC seabird distribution and abundance data (all trips) from ESAS database", 
                     "Observatoire Pelagis aerial surveys 2002-2021", "")
cetaceans_gbif <- cetaceans_gbif %>% 
  filter(!datasetName %in% remove_datasets)

# Filter by year
cetaceans_gbif <- cetaceans_gbif %>% 
  filter(year >= 1993, year < 2025)

# 2. Save cleaned dataset -------------------------------------------------

write_xlsx(cetaceans_gbif, path = "Products/1_L0/cetaceansgbif_cleaned_2025-01-13.xlsx")

