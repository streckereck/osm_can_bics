# The purpose of this script is to download OSM data for later processing.
# 
# * all "highways"
# * bike routes
# * identify which highways belong to routes, and what type
# 
# Starting with Vancouver and Montreal
# 
# The overall project flow is:
# 
# 1. Download data.
# 
#    * All highways.
#    * Bike routes.
#    * Identify which highways are part of bike routes.

library(tidyverse)
library(sf)
library(lwgeom)
library(osmdata)
library(utils)
library(RPostgreSQL)

source("./code/Can_BICS_OSM_functions.R")
source("./code/Can_BICS_OSM_classify.R")

# OSM2PSQL style file for importing osm data
style_path <- "./code/national/can_bics.style"

# download from Geofabrik: http://download.geofabrik.de/north-america/canada.html
provinces <- data.frame(
  name = c("AB",
          "BC",
          "MB",
          "NB",
          "NF",
          "NWT",
          "NS",
          "NV",
          "ON",
          "PEI",
          "QC",
          "SK",
          "YT"),
  filename = c("alberta-latest.osm.pbf",
               "british-columbia-latest.osm.pbf",
               "manitoba-latest.osm.pbf",
               "new-brunswick-latest.osm.pbf",
               "newfoundland-and-labrador-latest.osm.pbf",
               "northwest-territories-latest.osm.pbf",
               "nova-scotia-latest.osm.pbf",
               "nunavut-latest.osm.pbf",
               "ontario-latest.osm.pbf",
               "prince-edward-island-latest.osm.pbf",
               "quebec-latest.osm.pbf",
               "saskatchewan-latest.osm.pbf",
               "yukon-latest.osm.pbf"))

# testing: provinces <- provinces %>% filter(name %in% c("NWT"))

# download from geofabrik
source = c("http://download.geofabrik.de/north-america/canada/")
local_directory = c("C:/working/osm2pgsql/canada_data/")

for(i in 1:nrow(provinces)){
  print(provinces$name[i])
  print(provinces$filename[i])
  download.file(url = paste0(source, provinces$filename[i]),
                dest = paste0(local_directory, provinces$filename[i]),
                method = "curl",
                mode = "w")
}

# load into postGIS
# this depends on having post gis installed, and a default account named 
# postgres with no password (when executed in a local environment). 
# also download, extract, and add to path osm2pgsql 
# https://osm2pgsql.org/doc/install.html#installing-on-windows
# and modifications to default.style
# Tested on Windows 10.

for(i in 1:nrow(provinces)){
  print(i)
  print(provinces$name[i])
  print(provinces$filename[i])
  data_base_name <- paste0(provinces$name[i], "_osm_data")
  
  # create data base
  system(paste0("createdb -U postgres ", data_base_name))
  
  # add postgis extension
  system(paste0("psql -U postgres -d ", 
                data_base_name,  
                ' -c "CREATE EXTENSION postgis;"'))
  
  
  # load data
  system(paste0("osm2pgsql.exe -U postgres --slim -W -d ",
                data_base_name,
                " -S ", normalizePath(style_path),
                paste0(local_directory, provinces$filename[i])))
}

# create a central database for all data
central_db_name <- "Can_BICS_provinces"
system(paste0("createdb -U postgres ", central_db_name))
system(paste0("psql -U postgres -d ", 
              central_db_name,  
              ' -c "CREATE EXTENSION postgis;"'))


# pre-processing
# bike routes and roundabouts
for(i in 1:nrow(provinces)){
  print(provinces$name[i])
  print(provinces$filename[i])
  
  data_base_name <- paste0(provinces$name[i], "_osm_data")
  
  try(conn <- dbConnect(PostgreSQL(), 
                        dbname = data_base_name,
                        user = "postgres"))
  
  highways <- st_read(conn,
                      "planet_osm_line") %>%
    filter(! is.na(highway)) %>%
    st_transform(target_projection)
  
  routes_bike <- st_read(conn,
                         "planet_osm_rels") %>%
    filter(grepl("route,bicycle", tags)) %>%
    mutate(lcn = str_detect(tags, "network,lcn"),
           rcn = str_detect(tags, "network,rcn"),
           ncn = str_detect(tags, "network,ncn"),
           name = str_match(tags, 'name,"([^"]*)"')[,2])
  
  roundabouts <- st_read(conn,
                         "planet_osm_roads") %>%
    filter(junction %in% "roundabout") %>%
    st_transform(target_projection)
  
  dbDisconnect(conn) 
  
  system.time(highways <- in_bike_route_postgres(highways = highways,
                                                 bike_routes = routes_bike))
  
  # write to database
  
  library(RPostgreSQL)
  
  try(conn <- dbConnect(PostgreSQL(), 
                        dbname = central_db_name,
                        user = "postgres"))
  
  st_write(highways,  
           dsn = conn, 
           layer = paste0("highways", "_", provinces$name[i]),
           delete_dsn = T)
  
  st_write(roundabouts,  
           dsn = conn, 
           layer = paste0("roundabouts", "_", provinces$name[i]),
           delete_dsn = T)
  
  st_write(routes_bike,  
           dsn = conn, 
           layer = paste0("routes_bike", "_", provinces$name[i]),
           delete_dsn = T)
  
  dbDisconnect(conn) 
  print(paste0("Write to db", provinces$name[i]))
  
}
