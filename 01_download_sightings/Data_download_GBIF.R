# 1. Load packages & Shapefile --------------------------------------------

library(rgbif)
library(sf)
library(ggplot2)
library(readxl)
library(writexl)
library(dplyr)

shape <- read_sf("./Study Area/REDUCE_NEW_study_area.shp")
shape <- read_sf("./Study Area/PhD_studyarea/phd_studyarea.shp")

# 2. GBIF Account set up in .Renviron -------------------------------------

install.packages("usethis")
usethis::edit_r_environ()

# When ran, a new window (called ".Renviron") will appear. Here you need to type the credentials of your GBIF account:
# GBIF_USER="diegoferfer"
# GBIF_PWD="contraseñasegura1" if you are loging with google, you need to reset your gbif pwd with the "forgot password" option and use the new one here
# GBIF_EMAIL="diegoferfer0525@gmail.com"

# Once you have filled your credentials, click in the "Save" button in the .Renviron window and restart R.

Sys.getenv("GBIF_PWD")
Sys.getenv("GBIF_USER")

# with this code you can check if the credentials entered are ok without opening the .Renviron window. After restarting the R session and checking if your credentials have been implemented you should be able download data from GBIF. 


# 3. Data download -----------------------------

taxonKey <- name_backbone(name = "Cetacea")$usageKey
# Every taxon has its own key. With the function from above you can get the taxonkey of the species, family, order, etc. desired. 

occ_download(
  pred_and(pred("taxonKey", taxonKey), pred_within(simplified_wkt))
)

# The occ_download() function will place the download request and then execute it in the GBIF servers. In this case we asked for occurrence data of the taxon "Cetacea" within the limits of our simplified shapefile. Once you run this download request some text will appear in your console, giving you the information of the download and two different lines of code: the first one is to check the status of the download (the download happends in the background, could last from 15 minutes to 3 hours). The second code is to import the downloaded data in R (the zip file will be stored in your directory, while the function will unzip the files and load them in your environment).

# Citation at 13/01/2025
# https://www.gbif.org/citation-guidelines
# DOI: 10.15468/dl.e8scsn
# Citation:
#   GBIF Occurrence Download https://doi.org/10.15468/dl.e8scsn Accessed from R via rgbif (https://github.com/ropensci/rgbif) on 2025-01-13

occ_download_wait('0066134-241126133413365') # to check the status of the download 
d <- occ_download_get('0066134-241126133413365') %>%
  occ_download_import()
cetaceans_gbif <- d
write_xlsx(cetaceans_gbif, path = "Products/0_RawData/cetaceansgbif_raw_13012025.xlsx")
cetaceans_gbif <- read_excel("Products/0_RawData/cetaceansgbif_raw_13012025.xlsx")

