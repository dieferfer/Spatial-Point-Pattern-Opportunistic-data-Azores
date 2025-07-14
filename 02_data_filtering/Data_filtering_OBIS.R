
# 1. Data filtering -------------------------------------------------------
cetaceans_obis <- read_excel("Products/0_RawData/cetaceansobis_raw_13012025.xlsx")
unique(cetaceans_obis$scientificName)

# Delete observations that did not reach the id level of species. 
delete <- c("Delphinidae", 
            "Balaenoptera", 
            "Globicephala", 
            "Cetacea", 
            "Mesoplodon", 
            "Physeteroidea", 
            "Kogia", 
            "Hyperoodon", 
            "Mysticeti", 
            "Stenella", 
            "Kogiidae", 
            "Odontoceti", 
            "Ziphiidae", 
            "Balaenopteridae", 
            "Cephalorhynchus", 
            "Lagenorhynchus obscurus", 
            "Eubalaena")
cetaceans_obis <- cetaceans_obis %>% 
  filter(!scientificName %in% delete )

replacements <- c("Delphinus" = "Delphinus delphis", 
                  "Sousa" = "Sousa teuszii",
                  "Lagenorhynchus" = "Lagenorhynchus acutus",
                  "Phocoenidae" = "Phocoena phocoena",
                  "Balaenoptera musculus brevicauda" = "Balaenoptera musculus" 
                  # ,"Physeteridae" = "Physeter macrocephalus",
                  # "Tursiops truncatus truncatus" = "Tursiops truncatus",
                  # "Balaenoptera acutorostrata acutorostrata" = "Balaenoptera acutorostrata",
                  # "Globicephala melas melas" = "Globicephala melas",
                  # "Delphinus delphis delphis" = "Delphinus delphis",
                  # "Phocoena phocoena phocoena" = "Phocoena phocoena",
                  # "Megaptera novaeangliae australis" = "Megaptera novaeangliae",
                  # "Megaptera novaeangliae novaeangliae" = "Megaptera novaeangliae", 
                  # "Stenella attenuata attenuata" = "Stenella attenuata"
)


cetaceans_obis <- cetaceans_obis %>%
  mutate(scientificName = recode(scientificName, !!!replacements))
unique(cetaceans_obis$scientificName)

unique(cetaceans_obis$datasetName)
delete <- c("Historical distribution of whales shown by logbook records 1785-1913", 
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
            "EC-CWS Eastern Canada Seabirds at Sea" 
)

cetaceans_obis <- cetaceans_obis %>% 
  filter(!datasetName %in% delete )

cetaceans_obis <- cetaceans_obis %>%
  filter(year < 2025)

# add extra columns with the lat & lon. They original ones will be lost when transformed to sf. 
cetaceans_obis$lat <- round(cetaceans_obis$decimalLatitude,2)
cetaceans_obis$lon <- round(cetaceans_obis$decimalLongitude,2)

observations_sfcet <- st_as_sf(cetaceans_obis, coords = c("decimalLongitude", "decimalLatitude"), crs = st_crs(shape))
cortecet <- st_intersection(observations_sfcet, shape) # delete the observations out of the study area. 

ggplot() +
  geom_sf(data = shape, fill = "white", color = "black") + # shapefile
  geom_sf(data = cortecet, aes(color = family), size = 2) + # observations by family
  theme_minimal() +
  labs(title = "Cetacean observations OBIS",
       x = "Longitude",
       y = "Latitude") 

cortecet$publisher <- cortecet$recordedBy

write_xlsx(cortecet, path = "Products/1_L0/cetaceansobis_cleaned_13012025.xlsx")
cetaceans_obis <- read_excel("Products/1_L0/cetaceansobis_cleaned_13012025.xlsx")

# 2. Data visualization  --------------------------------------------------

data_count <- cetaceans_obis %>%
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


observations_sfcet <- st_as_sf(cetaceans_obis, coords = c("longitude", "latitude"), crs = st_crs(shape))
cortecet <- st_intersection(observations_sfcet, shape) # delete the observations out of the study area. 

ggplot() +
  geom_sf(data = shape, fill = "white", color = "black") + # shapefile
  geom_sf(data = cortecet, aes(color = family), size = 2) + # observations by family
  theme_minimal() +
  labs(title = "Cetacean observations OBIS",
       x = "Longitude",
       y = "Latitude") 


species_list <- as.character(unique(cetaceans_obis$scientificName))

# Directorio para guardar los gráficos
output_dir <- "C:/Users/ichu3/Dropbox/Diego/REDUCE/Plots OBIS/Species spatial occurrence"  # Cambia esto al directorio donde quieres guardar los gráficos
dir.create(output_dir, showWarnings = FALSE)

# Bucle para generar un gráfico por especie
for (i in species_list) {
  
  # Filtrar los datos para la especie actual (usando scientificName)
  species_data <- cetaceans_gbif %>% filter(scientificName == i)
  
  
  # Convertir el dataframe filtrado a un objeto sf
  if (nrow(species_data) > 0) {  # Solo proceder si hay datos para la especie
    species_data_sf <- st_as_sf(species_data, coords = c("lon", "lat"), crs = st_crs(shape))
    
    # Crear el gráfico de distribución espacial
    plot <- ggplot() +
      geom_sf(data = shape, fill = "white", color = "black") + # shapefile
      geom_sf(data = species_data_sf, size = 2, color = "blue") + # observations by species
      theme_minimal() +
      labs(title = paste("Distribución Espacial de", i),
           x = "Longitude",
           y = "Latitude")
    
    # Guardar el gráfico en un archivo JPEG
    ggsave(filename = paste(output_dir, "/", i, "_distribution.jpeg", sep = ""),
           plot = plot,
           width = 8, height = 6, units = "in")
  }
}
