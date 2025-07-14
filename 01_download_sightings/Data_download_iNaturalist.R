
# 1. Load Required Packages and Study Area Shapefile ----------------------

library(rinat)
library(sf)
library(ggplot2)
library(readxl)
library(writexl)
library(dplyr)
library(lubridate)

# Load study area shapefile (adjust path as needed)
study_area <- read_sf("./Study Area/PhD_studyarea/phd_studyarea.shp")

# 2. Download iNaturalist Cetacean Observations ---------------------------

# Download up to 10,000 iNaturalist observations for "Cetaceans" within study area
# 'simplified_shape' must be a bounding box or spatial object with CRS

observations <- get_inat_obs(
  taxon_name = "Cetaceans",
  geo = TRUE,
  bounds = simplified_shape,  # You need to define this spatial bounding box beforehand
  maxresults = 10000
)

# Save raw data to Excel
write_xlsx(observations, path = "Products/0_RawData/cetaceans_iNat_raw_2025-01-13.xlsx")

