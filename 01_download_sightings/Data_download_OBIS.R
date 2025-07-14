
# 1. Load Required Packages and Study Area Shapefile ----------------------

library(robis)
library(sf)
library(ggplot2)
library(readxl)
library(writexl)
library(dplyr)

# Load study area shapefile (adjust path as needed)
study_area <- read_sf("./Study Area/PhD_studyarea/phd_studyarea.shp")


# 2. Download OBIS Cetacean Occurrence Records ----------------------------

# Download cetacean occurrences from OBIS using a simplified WKT geometry
# 'simplified_wkt' must be a WKT string (geometry in EPSG:4326)

cetaceans_obis <- occurrence("Cetacea", geometry = simplified_wkt)

# Save raw data to Excel
write_xlsx(cetaceans_obis, path = "Products/0_RawData/cetaceans_OBIS_raw_2025-01-13.xlsx")

