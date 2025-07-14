# 1. Load packages & Shapefile --------------------------------------------

library(robis)
library(sf)
library(ggplot2)
library(readxl)
library(writexl)
library(dplyr)

shape <- read_sf("./Study Area/REDUCE_NEW_study_area.shp")
shape <- read_sf("./Study Area/PhD_studyarea/phd_studyarea.shp")

# 2. Data download --------------------------------------------------------

# This function gathers occurrence data for the specified taxon (it can be at species, genus, family, etc. level)
cetaceans_obis <- occurrence("Cetacea", geometry = simplified_wkt) 

write_xlsx(cetaceans_obis, path = "Products/0_RawData/cetaceansobis_raw_13012025.xlsx")
cetaceans_obis <- read_excel("Products/0_RawData/cetaceansobis_raw_13012025.xlsx")

