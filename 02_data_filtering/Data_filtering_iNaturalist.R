
# 1. Filter and Clean iNaturalist Observations ----------------------------

library(sf)
library(dplyr)
library(readxl)
library(writexl)
library(ggplot2)
library(lubridate)

# Load raw iNaturalist data
observations <- read_excel("Products/0_RawData/cetaceansiNat_raw_13012025.xlsx")

# Remove generic or ambiguous taxa
remove_taxa <- c("Delphinae", "Delphinidae", "Delphininae", "Balaenoptera", "Globicephala",
                 "Cetacea", "Delphinoidea", "Mesoplodon", "Ziphioidea", "Physeteroidea",
                 "Globicephalinae", "Mysticeti", "Stenella", "Balaenopteridae", "Odontoceti",
                 "Sotalia", "Ziphiidae", "Sagmatias obscurus", "Sotalia guianensis",
                 "Pontoporia blainvillei", "Sousa chinensis")

cetaceans_inat <- observations %>%
  filter(!scientific_name %in% remove_taxa)

# Standardize species names
replacements <- c("Tursiops" = "Tursiops truncatus",
                  "Delphinus" = "Delphinus delphis",
                  "Phocoenidae" = "Phocoena phocoena",
                  "Physeteridae" = "Physeter macrocephalus",
                  "Tursiops truncatus truncatus" = "Tursiops truncatus",
                  "Balaenoptera acutorostrata acutorostrata" = "Balaenoptera acutorostrata",
                  "Balaenoptera physalus physalus" = "Balaenoptera physalus",
                  "Globicephala melas melas" = "Globicephala melas",
                  "Delphinus delphis delphis" = "Delphinus delphis",
                  "Phocoena phocoena phocoena" = "Phocoena phocoena",
                  "Megaptera" = "Megaptera novaeangliae",
                  "Megaptera novaeangliae australis" = "Megaptera novaeangliae",
                  "Megaptera novaeangliae novaeangliae" = "Megaptera novaeangliae",
                  "Stenella attenuata attenuata" = "Stenella attenuata")
cetaceans_inat <- cetaceans_inat %>%
  mutate(scientific_name = recode(scientific_name, !!!replacements))

# Remove captive individuals
cetaceans_inat <- cetaceans_inat %>%
  filter(captive_cultivated != "true")

# 2. Add Family and Clean Metadata ----------------------------------------

gbif_data <- read_excel("./cetaceansgbif_cleaned_13012025.xlsx")
species_families <- gbif_data %>% select(scientificName, family) %>% distinct()

cetaceans_inat <- cetaceans_inat %>%
  left_join(species_families, by = c("scientific_name" = "scientificName"))

# Parse dates and filter by year
cetaceans_inat <- cetaceans_inat %>%
  mutate(datetime = ymd_hms(datetime),
         year = year(datetime),
         month = month(datetime),
         day = day(datetime),
         lon = round(longitude, 2),
         lat = round(latitude, 2),
         scientificName = scientific_name) %>%
  filter(year >= 1993, year < 2025)

# Spatial filtering
shape <- read_sf("./Study Area/PhD_studyarea/phd_studyarea.shp")
observations_sf <- st_as_sf(cetaceans_inat, coords = c("longitude", "latitude"), crs = st_crs(shape))
cetaceans_inat <- st_intersection(observations_sf, shape)

# Tag dataset and publisher
cetaceans_inat$datasetName <- "iNaturalist"
cetaceans_inat$publisher <- "iNaturalist"

# 3. Save cleaned dataset -------------------------------------------------

write_xlsx(cetaceans_inat, "Products/1_L0/cetaceansiNat_cleaned_13012025.xlsx")

