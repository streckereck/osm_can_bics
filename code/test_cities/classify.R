# This script classifies OSM data for the 15 test cities.
library(sf)
library(tidyverse)
source("./code/paths_and_variables.R")
source("./code/Can_BICS_OSM_functions.R")
source("./code/Can_BICS_OSM_classify.R")

# 1. Load data
selected_csds <- st_read("data/test_cities/selected_csds.gpkg")
reference_data <- st_read("data/test_cities/reference_data.gpkg")
highways_all <- st_read("data/test_cities/highways_all.gpkg")
roundabouts <- st_read("data/test_cities/roundabouts.gpkg")

# 1.5 testing
# select a single highway
# highways_all <- highways_all %>%
#   filter(osm_id %in% "366898817")

# 2. classify
highways_all_predicted <- classify_highways(highways_all,
                                             selected_csds,
                                             roundabouts) 
st_write(highways_all_predicted,
         "data/test_cities/highways_all_predicted.gpkg",
         delete_layer = T)

# 3. reference
reference_predicted <- classify_training_data(reference_data,
                                              highways_all_predicted)

st_write(reference_predicted, "data/test_cities/reference_predicted.gpkg",
         delete_layer = T)
