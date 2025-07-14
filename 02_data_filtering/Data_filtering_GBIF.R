
# 1. Data filtering -------------------------------------------------------
cetaceans_gbif <- read_excel("Products/0_RawData/cetaceansgbif_raw_13012025.xlsx")
unique(cetaceans_gbif$basisOfRecord) # identify the origin of the sightings 
delete <- c("PRESERVED_SPECIMEN", "FOSSIL_SPECIMEN", "MATERIAL_SAMPLE", "MATERIAL_CITATION", "LIVING_SPECIMEN") # remove not useful origins
cetaceans_gbif <- cetaceans_gbif %>% 
  filter(!basisOfRecord %in% delete )

unique(cetaceans_gbif$family) # delete extincted families or NA
delete <- c("Eschrichtiidae", "Platanistidae", "", "Lipotidae", NA)
cetaceans_gbif <- cetaceans_gbif %>% 
  filter(!family %in% delete )
unique(cetaceans_gbif$family)
cetaceans_gbif$family[cetaceans_gbif$family == "Hyperoodontidae"] <- "Ziphiidae" # change the name for the beaked whales family

cetaceans_gbif$lat <- round(cetaceans_gbif$decimalLatitude,2)
cetaceans_gbif$lon <- round(cetaceans_gbif$decimalLongitude,2)

observations_sfcetgbif <- st_as_sf(cetaceans_gbif, coords = c("decimalLongitude", "decimalLatitude"), crs = st_crs(shape))
cortecet <- st_intersection(observations_sfcetgbif, shape) # delete the observations out of the study area. 

ggplot() +
  geom_sf(data = shape, fill = "white", color = "black") + # shapefile
  geom_sf(data = cortecet, aes(color = family), size = 2) + # observations by family
  theme_minimal() +
  labs(title = "Cetacean observations GBIF",
       x = "Longitude",
       y = "Latitude") 

cetaceans_gbif <- cortecet
cetaceans_gbif$scientificName <- sub("^([A-Za-z]+\\s+[a-z]+).*", "\\1", cetaceans_gbif$scientificName)
cetaceans_gbif$scientificName <- ifelse(grepl("^[A-Za-z]+\\s+\\p{L}+[,\\s]*\\d{4}$", 
                                              cetaceans_gbif$scientificName, perl = TRUE),
                                        # Si solo hay género y autor + año, eliminar el autor y el año
                                        sub("\\s+\\p{L}+.*$", "", cetaceans_gbif$scientificName, perl = TRUE),
                                        # Si ya tiene el género + especie, dejarlo como está
                                        cetaceans_gbif$scientificName)

delete <- c("Phocoena spinipinnis",
            "Balaenoptera", 
            "Delphinidae", 
            "Globicephala", 
            "Delphinus", 
            "Mesoplodon", 
            "Stenella", 
            "Ziphiidae", 
            "Kogiidae", 
            "Balaenopteridae", 
            "Hyperoodontidae", 
            "Kogia", 
            "Hyperoodon", 
            "Cephalorhynchus", 
            "Neophocaena phocaenoides", 
            "Phocoenidae",
            "Orcaella brevirostris" )

cetaceans_gbif <- cetaceans_gbif %>% 
  filter(!scientificName %in% delete )

replacements <- c("Tursiops truncantus" = "Tursiops truncatus",
                  "Sousa lentiginosa" = "Sousa teuszii",
                  "Physeter catodon" = "Physeter macrocephalus",
                  "Tursiops" = "Tursiops truncatus",
                  "Globicephala melaena" = "Globicephala melas",
                  "Delphinus capensis" = "Delphinus delphis",
                  "Lagenorhynchus" = "Lagenorhynchus acutus")
cetaceans_gbif <- cetaceans_gbif %>%
  mutate(scientificName = recode(scientificName, !!!replacements))
unique(cetaceans_gbif$scientificName)

delete <- c("Administración de Parques Nacionales, Argentina", 
            "Ministerio del Medio Ambiente de Chile", 
            "National Museum of Nature and Science, Japan", 
            "Questagame", 
            "The National Institute of Water and Atmospheric Research (NIWA)", 
            "Finnish Biodiversity Information Facility")

cetaceans_gbif <- cetaceans_gbif %>% 
  filter(!publisher %in% delete )

delete <- c("Historical distribution of whales shown by logbook records 1785-1913", 
            "PELACUS", 
            "CODA cetacean sightings on primary platform of vessel surveys 2007", 
            "CETUS: Cetacean monitoring surveys in the Eastern North Atlantic", 
            "SCANS II cetacean sightings on primary platform of vessel surveys 2005", 
            "Historical strandings of cetaceans on the Portuguese coast", 
            "Cetacean occurrence off the west central Portugal coast from boat-based surveys 2007-2008", 
            "Cetacean monitoring data collected along fixed transect using ferries as platforms in the Strait of Gibraltar, 2018, FLT Med Network", 
            "PIROP Northwest Atlantic 1965-1992", "CODA cetacean sightings on tracker platform of vessel surveys 2007", 
            "SCANS II cetacean sightings on tracker platform of vessel surveys 2005", 
            "Allied Humpback Whale Catalogue, 1976 - 2003", 
            "Alnitak-Alnilam Cetaceans and sea turtles surveys off Southern Spain",
            "Inforbiomares : Biodiversity Census",
            "InforBiomares",  
            "Tethys Research Institute shipboard survey cetacean sightings 1986-2012", 
            "Observatoire Pelagis boat surveys 2003-2021",
            "Observatoire Pelagis sightings from fishery surveys 2004-2009",
            "European Seabirds At Sea (ESAS)",
            "JNCC seabird distribution and abundance data (all trips) from ESAS database",
            "Observatoire Pelagis aerial surveys 2002-2021",  
            "")

cetaceans_gbif <- cetaceans_gbif %>% 
  filter(!datasetName %in% delete )
unique(cetaceans_gbif$datasetName)

cetaceans_gbif <- cetaceans_gbif %>%
  filter(year >= 1993)
cetaceans_gbif <- cetaceans_gbif %>%
  filter(year < 2025)

write_xlsx(cetaceans_gbif, path = "Products/1_L0/cetaceansgbif_cleaned_13012025.xlsx")
cetaceans_gbif <- read_excel("Products/1_L0/cetaceansgbif_cleaned_13012025.xlsx")


# 2. Data visualization ---------------------------------------------------

# Create barplots for every family at species level 

data_count <- cetaceans_gbif %>%
  group_by(scientificName, family) %>%
  summarize(value = n()) %>%
  ungroup() #get the count of each family

groups <- unique(data_count$family)
max_value <- max(data_count$value)
# Iterate every family and create a barplot for each one of them
for (grp in groups) {
  
  # Filter data by the actual family
  data_grp <- data_count %>% filter(family == grp)
  
  # barplot
  p <- ggplot(data_grp, aes(x = scientificName, y = value, fill = scientificName)) +
    geom_bar(stat = "identity") +      # Bar made from the counts
    geom_text(aes(label = value),      # add number of obs on top
              vjust = -0.5,            
              size = 3.5) +
    ylim(0, max_value * 1.1) + #set the ylim to the maximum count value
    theme_minimal() +                
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate labels
      legend.position = "none"                           
    ) +
    labs(x = " ", y = "Count", title = paste("Frequency of Species in", grp))
  
  print(p)
  ggsave(filename = paste0("species_frequency_", grp, ".jpeg"), plot = p, width = 8, height = 6)
}

# # Crear una lista única de especies
# 
# species_list <- as.character(unique(cetaceans_gbif$scientificName))
# 
# # Directorio para guardar los gráficos
# output_dir <- "C:/Users/ichu3/Dropbox/Diego/REDUCE/Plots GBIF/Species spatial occurrence"  # Cambia esto al directorio donde quieres guardar los gráficos
# dir.create(output_dir, showWarnings = FALSE)
# 
# # Bucle para generar un gráfico por especie
# for (i in species_list) {
#   
#   # Filtrar los datos para la especie actual (usando scientificName)
#   species_data <- cetaceans_gbif %>% filter(scientificName == i)
# 
#   
#   # Convertir el dataframe filtrado a un objeto sf
#   if (nrow(species_data) > 0) {  # Solo proceder si hay datos para la especie
#     species_data_sf <- st_as_sf(species_data, coords = c("lon", "lat"), crs = st_crs(shape))
#     
#     # Crear el gráfico de distribución espacial
#     plot <- ggplot() +
#       geom_sf(data = shape, fill = "white", color = "black") + # shapefile
#       geom_sf(data = species_data_sf, size = 2, color = "blue") + # observations by species
#       theme_minimal() +
#       labs(title = paste("Distribución Espacial de", i),
#            x = "Longitude",
#            y = "Latitude")
#     
#     # Guardar el gráfico en un archivo JPEG
#     ggsave(filename = paste(output_dir, "/", i, "_distribution.jpeg", sep = ""),
#            plot = plot,
#            width = 8, height = 6, units = "in")
#   }
# }
# 
# 
# cetaceans_gbif <- st_as_sf(cetaceans_gbif, coords = c("lon", "lat"), crs = st_crs(shape))
# a <- ggplot() +
#   geom_sf(data = shape, fill = "white", color = "black") + # shapefile
#   geom_sf(data = cetaceans_gbif, aes(color = family), size = 2) + # observations by family
#   theme_minimal() +
#   labs(title = "Cetacean observations GBIF",
#        x = "Longitude",
#        y = "Latitude") 
# ggsave(filename = paste("Cetfamilies_spatial_distribution.jpeg"),
#        plot = a,
#        width = 8, height = 6, units = "in")
