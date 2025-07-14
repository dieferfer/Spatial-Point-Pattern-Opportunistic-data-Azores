# Environmental Variables from CMEMS - Download and Processing
# Author: Diego Fernández
# Date: 14/07/2025
# Description: This script (adapted from David Ruiz github) downloads 3D environmental variables from CMEMS,
# stores them in a structured folder system, and computes monthly and seasonal means.

# 1. Load required libraries ----------------------------------------------

library(reticulate)
library(lubridate)
library(dplyr)
library(readr)
library(stringr)
library(terra)

# 2. Load catalog and define paths ----------------------------------------

input_data <- "../Environmental Variables"
catalog <- read_delim(file.path(input_data, "catalog_env_var.csv"), delim = ";", 
                      escape_double = FALSE, trim_ws = TRUE)
catalog <- catalog %>%
  mutate(across(c(xmin, xmax, ymin, ymax, depth_min, depth_max), ~as.numeric(gsub(",", ".", .))))

# 3. CMEMS login (via reticulate) -----------------------------------------

use_virtualenv("cmems", required = TRUE)
cm <- import("copernicusmarine")

# Prompt for credentials (do not store them in the script)
username <- "Enter CMEMS username: "
password <- "Enter CMEMS password: "
cm$login(username, password)

# 4. Download full .nc file per variable ----------------------------------

destination_folder <- file.path(input_data, "cmems")
if (!dir.exists(destination_folder)) dir.create(destination_folder, recursive = TRUE)

for (i in 1:nrow(catalog)) {
  product <- catalog[i, ]
  dir_path <- file.path(destination_folder, product$service, product$layer, product$var_name)
  if (!file.exists(dir_path)) dir.create(dir_path, recursive = TRUE)
  
  file_name <- paste0(product$var_name, "_", product$product_type, "_full.nc")
  
  if (product$product_type == "Reanalysis") {
    start_date <- "1993-01-01 00:00:00"
    end_date   <- "2022-12-01 00:00:00"
  } else {
    start_date <- "2023-01-01 00:00:00"
    end_date   <- "2024-12-01 00:00:00"
  }
  
  message("\U0001F504 Downloading ", file_name)
  cm$subset(
    dataset_id = product$layer,
    start_datetime = start_date,
    end_datetime = end_date,
    variables = list(product$variable),
    minimum_longitude = product$xmin,
    maximum_longitude = product$xmax,
    minimum_latitude = product$ymin,
    maximum_latitude = product$ymax,
    minimum_depth = product$depth_min,
    maximum_depth = product$depth_max,
    output_filename = file_name,
    output_directory = dir_path
  )
}


# 5. Compute interannual monthly means ------------------------------------

monthly_dir <- file.path(destination_folder, "monthly_means")
dir.create(monthly_dir, showWarnings = FALSE)

nc_files <- list.files(destination_folder, pattern = "full\\.nc$", recursive = TRUE, full.names = TRUE)

calc_monthly_mean <- function(nc_path) {
  r <- rast(nc_path)
  time_vals <- time(r)
  months <- format(time_vals, "%m")
  monthly_means <- lapply(sprintf("%02d", 1:12), function(m) mean(r[[which(months == m)]], na.rm = TRUE))
  r_out <- rast(monthly_means)
  names(r_out) <- month.abb
  var_name <- str_extract(basename(nc_path), "^[^_]+")
  tipo <- if (grepl("Analysis", basename(nc_path))) "Analysis" else "Reanalysis"
  out_path <- file.path(monthly_dir, paste0(var_name, "_", tipo, "_monthly_mean.nc"))
  writeCDF(r_out, filename = out_path, overwrite = TRUE)
  message("\U00002705 Saved monthly mean for ", var_name)
}

invisible(lapply(nc_files, calc_monthly_mean))

# 6. Compute seasonal means (Winter/Summer) -------------------------------

seasonal_dir <- file.path(destination_folder, "seasonal_means")
dir.create(seasonal_dir, showWarnings = FALSE)

get_season <- function(month_num) {
  switch(as.character(month_num),
         "12" = "winter", "1" = "winter", "2" = "winter",
         "3" = "winter", "4" = "summer", "5" = "summer",
         "6" = "summer", "7" = "summer", "8" = "summer",
         "9" = "summer", "10" = "winter", "11" = "winter")
}

calc_seasonal_mean <- function(monthly_path) {
  r <- rast(monthly_path)
  names(r) <- paste0("month_", 1:12)
  variable <- str_extract(basename(monthly_path), "^[^_]+")
  
  for (season in c("winter", "summer")) {
    if (season == "winter") {
      idx <- c(10,11,12, 1,2,3)
    } else {
      idx <- c(4,5,6, 7,8,9)
    }
    season_mean <- mean(r[[idx]], na.rm = TRUE)
    out_file <- file.path(seasonal_dir, paste0(variable, "_", season, ".nc"))
    writeCDF(season_mean, filename = out_file, overwrite = TRUE)
    message("\U0001F4BE Saved seasonal mean: ", out_file)
  }
}

monthly_files <- list.files(monthly_dir, pattern = "_monthly_mean\\.nc$", full.names = TRUE)
invisible(lapply(monthly_files, calc_seasonal_mean))

# 7. Quality control summary ----------------------------------------------

qc_check <- function(path) {
  r <- tryCatch(rast(path), error = function(e) return(NULL))
  if (is.null(r)) return(data.frame(file = basename(path), error = "Failed to load"))
  data.frame(
    file = basename(path),
    layers = nlyr(r),
    extent = paste(round(ext(r), 2), collapse = ", "),
    resolution = paste(res(r), collapse = " x "),
    crs = crs(r, describe = TRUE),
    error = NA
  )
}

monthly_qc <- bind_rows(lapply(monthly_files, qc_check))
seasonal_files <- list.files(seasonal_dir, pattern = "\.nc$", full.names = TRUE)
seasonal_qc <- bind_rows(lapply(seasonal_files, qc_check))

# Print summaries
print(monthly_qc)
print(seasonal_qc)

message("\n\U0001F389 CMEMS environmental processing completed!")
