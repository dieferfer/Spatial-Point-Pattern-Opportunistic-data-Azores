
# 1. Load Required Packages and Study Area Shapefile ----------------------

library(rgbif)
library(sf)
library(ggplot2)
library(readxl)
library(writexl)
library(dplyr)

# Load study area shapefile (adjust path as needed)
study_area <- read_sf("./Study Area/study_area_azores.shp")

# 2. GBIF Account Setup (only needed once) --------------------------------

# If not installed yet:
# install.packages("usethis")

# Open .Renviron to add your GBIF credentials:
# usethis::edit_r_environ()

# Example credentials (add these manually to the file):
# GBIF_USER="your_username"
# GBIF_PWD="your_password"
# GBIF_EMAIL="your_email@example.com"

# After saving the file, restart R and check that credentials were loaded:
Sys.getenv("GBIF_USER")
Sys.getenv("GBIF_PWD")

# 3. GBIF Data Download (Cetacea occurrences) -----------------------------

# Retrieve taxon key for Cetacea
taxon_key <- name_backbone(name = "Cetacea")$usageKey

# Prepare WKT geometry for spatial filter (must be < 1500 characters)
# simplified_wkt <- your_simplified_wkt_string

# Submit download request (runs on GBIF servers)
occ_download(
  pred_and(
    pred("taxonKey", taxon_key),
    pred_within(simplified_wkt)
  )
)

# Once submitted, you will get a download key (example below)
# Check download status (may take 15 minutes to several hours)
occ_download_wait("0066134-241126133413365")

# Download and import data
gbif_data <- occ_download_get("0066134-241126133413365") %>%
  occ_download_import()

# Save as Excel
write_xlsx(gbif_data, path = "Products/0_RawData/cetaceans_gbif_raw_2025-01-13.xlsx")

# Citation (Required by GBIF) ---------------------------------------------

# Accessed: 2025-01-13
# DOI: 10.15468/dl.e8scsn
# Citation: GBIF Occurrence Download https://doi.org/10.15468/dl.e8scsn 
# Accessed via rgbif (https://github.com/ropensci/rgbif)
