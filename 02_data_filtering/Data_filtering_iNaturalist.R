
# 1. Data filtering -------------------------------------------------------
observations <- read_excel("Products/0_RawData/cetaceansiNat_raw_13012025.xlsx")
unique(observations$scientific_name)
delete <- c("Delphinae", "Delphinidae", "Delphininae","Balaenoptera", "Globicephala", "Cetacea", "Delphinoidea", "Mesoplodon", "Ziphioidea", "Physeteroidea", "Globicephalinae", "Mysticeti", "Stenella", "Balaenopteridae", "Odontoceti", "Sotalia", "Ziphiidae", "Sagmatias obscurus", "Sotalia guianensis", "Pontoporia blainvillei", "Sousa chinensis" )

cetaceans_inat <- observations %>% 
  filter(!scientific_name %in% delete )

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
                  "Stenella attenuata attenuata" = "Stenella attenuata",
                  "Megaptera" = "Megaptera novaeangliae")


cetaceans_inat <- cetaceans_inat %>%
  mutate(scientific_name = recode(scientific_name, !!!replacements))
unique(cetaceans_inat$scientific_name)
cetaceans_inat <- cetaceans_inat %>% 
  filter(!captive_cultivated %in% "true" )

# 2. Data visualization ---------------------------------------------------

# create family column 
cetaceans_gbif <- read_excel("./cetaceansgbif_cleaned_13012025.xlsx")
balaenidae <- cetaceans_gbif %>% filter(family == "Balaenidae")
balaenidae_sp <- unique(balaenidae$scientificName)
balaenopteridae <- cetaceans_gbif %>% filter(family == "Balaenopteridae")
balaenopteridae_sp <- unique(balaenopteridae$scientificName)
delphinidae <- cetaceans_gbif %>% filter(family == "Delphinidae")
delphinidae_sp <- unique(delphinidae$scientificName)
kogiidae <- cetaceans_gbif %>% filter(family == "Kogiidae")
kogiidae_sp <- unique(kogiidae$scientificName)
phocoenidae <- cetaceans_gbif %>% filter(family == "Phocoenidae")
phocoenidae_sp <- unique(phocoenidae$scientificName)
physeteridae <- cetaceans_gbif %>% filter(family == "Physeteridae")
physeteridae_sp <- unique(physeteridae$scientificName)
ziphiidae <- cetaceans_gbif %>% filter(family == "Ziphiidae")
ziphiidae_sp <- unique(ziphiidae$scientificName)

cetaceans_inat <- cetaceans_inat %>%
  mutate(family = case_when(
    scientific_name %in% balaenidae_sp ~ "Balaenidae",
    scientific_name %in% balaenopteridae_sp ~ "Balaenopteridae",
    scientific_name %in% delphinidae_sp ~ "Delphinidae",
    scientific_name %in% kogiidae_sp ~ "Kogiidae",
    scientific_name %in% phocoenidae_sp ~ "Phocoenidae",
    scientific_name %in% physeteridae_sp ~ "Physeteridae",
    scientific_name %in% ziphiidae_sp ~ "Ziphiidae",
    TRUE ~ "Other" # Asignar a "Other" si no está en la lista
  ))


cetaceans_inat$datetime <- ymd_hms(cetaceans_inat$datetime)
cetaceans_inat$year <- year(cetaceans_inat$datetime)
cetaceans_inat$month <- month(cetaceans_inat$datetime)
cetaceans_inat$day <- day(cetaceans_inat$datetime)
cetaceans_inat <- cetaceans_inat %>%
  filter(year >= 1993)
cetaceans_inat <- cetaceans_inat %>%
  filter(year < 2025)
cetaceans_inat$lon <- round(cetaceans_inat$longitude,2)
cetaceans_inat$lat <- round(cetaceans_inat$latitude,2)
cetaceans_inat$scientificName <- cetaceans_inat$scientific_name 
observations_sfcet <- st_as_sf(cetaceans_inat, coords = c("longitude", "latitude"), crs = st_crs(shape))
cortecet <- st_intersection(observations_sfcet, shape)
ggplot() +
  geom_sf(data = shape, fill = "white", color = "black") + # Plot del shapefile
  geom_sf(data = corteinat, color = "blue" ,size = 2) + # Plot de los puntos de observaciones
  theme_minimal() +
  labs(title = "Cetacean observations iNaturalist",
       x = "Longitud",
       y = "Latitud")
cortecet$datasetName <- rep("iNaturalist", nrow(cortecet))
cortecet$publisher <- rep("iNaturalist", nrow(cortecet))

write_xlsx(cortecet, path = "Products/1_L0/cetaceansiNat_cleaned_13012025.xlsx")
cetaceans_inat <- read_excel("Products/1_L0/cetaceansiNat_cleaned_13012025.xlsx")


data_count <- cetaceans_inat %>%
  group_by(scientific_name, family) %>%
  summarize(value = n()) %>%
  ungroup() #get the count of each family

groups <- unique(data_count$family)
max_value <- max(data_count$value)

# Iterate every family and create a barplot for each one of them
for (grp in groups) {
  
  # Filter data by the actual family
  data_grp <- data_count %>% filter(family == grp)
  
  # barplot
  p <- ggplot(data_grp, aes(x = scientific_name, y = value, fill = scientific_name)) +
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

cetaceans_inat <- st_as_sf(cetaceans_inat, coords = c("lon", "lat"), crs = st_crs(shape))
colors <- c("Balaenopteridae" = "#C49A00", # Azul
            "Delphinidae" = "#53B400", # Verde
            "Kogiidae" = "#00C094", # Morado
            "Phocoenidae" = "#00B6EB", # Rosa
            "Physeteridae" = "#A58AFF", # Amarillo
            "Ziphiidae" = "#FB61D7") # Naranja
p <- ggplot() +
  geom_sf(data = shape, fill = "white", color = "black") + # shapefile
  geom_sf(data = cetaceans_inat, aes(color = family), size = 2) + # observations by family
  scale_color_manual(values = colors) + # observations by family
  theme_minimal() +
  labs(title = "Cetacean observations OBIS",
       x = "Longitude",
       y = "Latitude") 
ggsave(filename = "Cetfamilies_spatial_distribution.jpeg", plot = p, width = 8, height = 6)


species_list <- as.character(unique(cetaceans_inat$scientific_name))

# Directorio para guardar los gráficos
output_dir <- "C:/Users/ichu3/Dropbox/Diego/REDUCE/Plots iNaturalist/Species spatial occurrence"  # Cambia esto al directorio donde quieres guardar los gráficos
dir.create(output_dir, showWarnings = FALSE)

# Bucle para generar un gráfico por especie
for (i in species_list) {
  
  # Filtrar los datos para la especie actual (usando scientificName)
  species_data <- cetaceans_inat %>% filter(scientific_name == i)
  
  
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


