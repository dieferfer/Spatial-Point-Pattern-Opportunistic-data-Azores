# 1. Load Required Packages, Shapefile, and Cleaned Datasets -------------

library(dplyr)
library(readxl)
library(writexl)
library(sf)

# Load the study area shapefile
shape <- read_sf("./Study Area/PhD_studyarea/phd_studyarea.shp")

# Load cleaned datasets
cetaceans_gbif <- read_excel("Products/1_L0/cetaceansgbif_cleaned_13012025.xlsx")
cetaceans_inat <- read_excel("Products/1_L0/cetaceansiNat_cleaned_13012025.xlsx")
cetaceans_obis <- read_excel("Products/1_L0/cetaceansobis_cleaned_13012025.xlsx")


# 2. Harmonize Columns and Merge Datasets --------------------------------

# Identify common columns across all datasets
common_columns <- Reduce(intersect, list(
  colnames(cetaceans_gbif),
  colnames(cetaceans_obis),
  colnames(cetaceans_inat)
))
# Remove columns not relevant for merging
common_columns <- setdiff(common_columns, c("FID", "geometry"))

# Subset each dataset to only keep common columns
cetaceans_gbif_common <- cetaceans_gbif %>% select(all_of(common_columns))
cetaceans_obis_common <- cetaceans_obis %>% select(all_of(common_columns))
cetaceans_inat_common <- cetaceans_inat %>% select(all_of(common_columns))

# Ensure consistent column types across datasets
standardize_types <- function(df) {
  df %>%
    mutate(
      year = as.integer(year),
      month = as.integer(month),
      day = as.integer(day),
      lat = as.numeric(lat),
      lon = as.numeric(lon),
      scientificName = as.character(scientificName),
      datasetName = as.character(datasetName),
      publisher = as.character(publisher)
    )
}
cetaceans_gbif_common <- standardize_types(cetaceans_gbif_common)
cetaceans_obis_common <- standardize_types(cetaceans_obis_common)
cetaceans_inat_common <- standardize_types(cetaceans_inat_common)

# Merge the three datasets
cetaceans_combined <- bind_rows(
  cetaceans_gbif_common,
  cetaceans_obis_common,
  cetaceans_inat_common
)


# 3. Remove Duplicate Records ---------------------------------------------

# Keep only unique records by date, location, and species
cetaceans_cleaned <- cetaceans_combined %>%
  distinct(year, month, day, lat, lon, scientificName, .keep_all = TRUE)

# Check the number of records before and after
cetaceans_cleaned # From 257,243 records, 84,826 remain after deduplication

# Save cleaned merged dataset
write_xlsx(cetaceans_cleaned, path = "Products/1_L0/cetaceans_combined_13012025.xlsx")
cetaceans_combined <- read_excel("Products/1_L0/cetaceans_combined_13012025.xlsx")
