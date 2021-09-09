# This script classifies OSM data for the 15 sample cities.
library(sf)
library(tidyverse)
library(raster)
source("./code/Can_BICS_OSM_functions.R")

# manually specify the location of the landcover dataset downloaded from:
# source: https://open.canada.ca/data/en/dataset/4e615eae-b90c-420b-adee-2ca35896caf6
# https://ftp.maps.canada.ca/pub/nrcan_rncan/Land-cover_Couverture-du-sol/canada-landcover_canada-couverture-du-sol/CanadaLandcover2015.zip

# due to the large size (2GB), this is added as an additional step to add more
# control to choose a good place to download and store the data
landcover <- raster("C:/working/landcover/CAN_LC_2015_CAL.tif")
source("./code/Can_BICS_OSM_classify.R")


# 1. Load data
selected_csds <- st_read("data/sample_cities/selected_csds.gpkg")
reference_data <- st_read("data/sample_cities/reference_data.gpkg")
highways_all <- st_read("data/sample_cities/highways_all.gpkg")
roundabouts <- st_read("data/sample_cities/roundabouts.gpkg")

# classify

highways_all_predicted <- classify_highways(highways_all,
                                             selected_csds,
                                             roundabouts) 
st_write(highways_all_predicted,
         "data/sample_cities/highways_all_predicted.gpkg",
         delete_layer = T)

# reference
reference_predicted <- classify_training_data(reference_data,
                                              highways_all_predicted)

st_write(reference_predicted, "data/sample_cities/reference_predicted.gpkg",
         delete_layer = T)
