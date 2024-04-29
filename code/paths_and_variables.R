# this file contains local paths and variables 

################################################################################
# Census mapper api keys
################################################################################
# example:
# options(cancensus.api_key=<your CensusMapper API key>)
# options(cancensus.cache_path = <your long term cache path>)
# source("./code/personal_paths_and_variables.R")


################################################################################
# Consistent labels
################################################################################
infra_levels <- c("Bike Path",
                  "Cycle Track",
                  "Local Street Bikeway",
                  "Multi-Use Path",
                  "Painted Bike Lane",
                  "Non-Conforming")

infra_comfort_levels <- c("1. High Comfort",
                          "2. Medium Comfort",
                          "3. Low Comfort",
                          "Non-Conforming")

presence_levels <- c("Can-BICS",
                     "Non-Conforming")

################################################################################
# CSD boundaries
################################################################################
# source: shapefile of census subdivisions digital boundary file
# https://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/bound-limit-2016-eng.cfm
csds <- st_read("C:/Users/16043/Documents/basemap/census_2021/census_csd_boundary/lcsd000a21a_e.shp")
pr <- st_read("C:/Users/16043/Documents/basemap/census_2021/census_pr_boundary/lpr_000a21a_e.shp")

# add the province name to the csds
csds <- csds %>%
  left_join(pr %>% 
              st_drop_geometry() %>%
              dplyr::select(PRUID,
                            PRNAME))

################################################################################
# Landcover
################################################################################
# manually specify the location of the landcover dataset downloaded from:
# source: https://open.canada.ca/data/en/dataset/4e615eae-b90c-420b-adee-2ca35896caf6
# https://ftp.maps.canada.ca/pub/nrcan_rncan/Land-cover_Couverture-du-sol/canada-landcover_canada-couverture-du-sol/CanadaLandcover2015.zip

# due to the large size (2GB), this is added as an additional step to add more
# control to choose a good place to download and store the data
landcover <- raster("C:/working/landcover/CAN_LC_2015_CAL.tif")

wkt <- sf::st_crs(3978)[[2]]
proj4string(landcover) <- sp::CRS(wkt)


landcover_terra <- rast("C:/working/landcover/CAN_LC_2015_CAL.tif")

################################################################################
# National data download directory
################################################################################
national_data_local_directory = c("C:/working/osm2pgsql/canada_data/")

################################################################################
# Projections
################################################################################
# stats can lambert
target_projection <- 3347

# wgs 84
wgs_84_projection <- 4326