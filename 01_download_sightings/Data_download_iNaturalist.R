# 1. Load packages & Shapefile --------------------------------------------

library(rinat)
library(sf)
library(ggplot2)
library(readxl)
library(writexl)
library(dplyr)
library(lubridate)

shape <- read_sf("./Study Area/REDUCE_NEW_study_area.shp")
shape <- read_sf("./Study Area/PhD_studyarea/phd_studyarea.shp")

# 2. Data download --------------------------------------------------------

observations <- get_inat_obs(taxon_name = "Cetaceans",
                             geo = TRUE,
                             bounds = simplified_shape,
                             maxresults = 10000)
write_xlsx(observations, path = "Products/0_RawData/cetaceansiNat_raw_13012025.xlsx")
observations <- read_excel("Products/0_RawData/cetaceansiNat_raw_13012025.xlsx")

