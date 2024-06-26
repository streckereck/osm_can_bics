# The purpose of this script is to download OSM data for later processing.
# 
# * all "highways"
# * bike routes
# * identify which highways belong to routes, and what type

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

# Switch from Geofabrik to Planet OSM
# Download planet OSM file from: https://planet.osm.org/pbf/
# Version 3 uses version 230123 to match Strava Metro

# download stats can province boundary files from: https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/index2021-eng.cfm?year=21


provinces <- data.frame(
  name = c("ab",
           "bc",
           "mb",
           "nb",
           "nl",
           "nwt",
           "ns",
           "nvt",
           "on",
           "pei",
           "qc",
           "sk",
           "yt")) %>%
  arrange(name)

create_boundary_files <- F

if(create_boundary_files){
# create simplified provinces
provinces_boundary <- st_read("C:/Users/16043/Documents/basemap/census_2021/census_pr_boundary/lpr_000a21a_e.shp") %>%
  arrange(PREABBR)

provinces_boundary$PREABBR[which(provinces_boundary$PRENAME %in% "Nunavut")] <- "N.V.T."

provinces_boundary <- provinces_boundary %>%
  arrange(PREABBR)

provinces_boundary$name <- provinces$name

buffer_dist <- 1000

for(i in 1:nrow(provinces)){
  province_simple <- st_buffer(provinces_boundary[i, ],
                               buffer_dist) %>%
    st_simplify(
      preserveTopology = T,
      dTolerance = buffer_dist) %>%
    mutate(name = provinces$name[i]) %>%
    st_cast("MULTIPOLYGON") %>%
    st_write(paste0("C:/working/planet_osm/", 
                    provinces$name[i],
                    ".gpkg"),
             delete_dsn = T)
}}

# manual step: use QGIS to export to POLY files
# use the QGIS Export OSM Poly plugin to export using the name field 

# open a commmand prompt in the directory: C:\\working\\planet_osm
# use osmosis to create a pbf for each province and territory:

# osmosis --read-pbf file=E:\planet-240115.osm.pbf --bounding-polygon file=ab.poly --write-pbf ab_2024.pbf
# osmosis --read-pbf file=E:\planet-240115.osm.pbf --bounding-polygon file=bc.poly --write-pbf bc_2024.pbf
# osmosis --read-pbf file=E:\planet-240115.osm.pbf --bounding-polygon file=mb.poly --write-pbf mb_2024.pbf
# osmosis --read-pbf file=E:\planet-240115.osm.pbf --bounding-polygon file=nb.poly --write-pbf nb_2024.pbf
# osmosis --read-pbf file=E:\planet-240115.osm.pbf --bounding-polygon file=nl.poly --write-pbf nl_2024.pbf
# osmosis --read-pbf file=E:\planet-240115.osm.pbf --bounding-polygon file=ns.poly --write-pbf ns_2024.pbf
# osmosis --read-pbf file=E:\planet-240115.osm.pbf --bounding-polygon file=on.poly --write-pbf on_2024.pbf
# osmosis --read-pbf file=E:\planet-240115.osm.pbf --bounding-polygon file=pei.poly --write-pbf pei_2024.pbf


# osmosis --read-pbf file=E:\planet-240115.osm.pbf --bounding-polygon file=qc.poly --write-pbf qc_2024.pbf
# osmosis --read-pbf file=E:\planet-240115.osm.pbf --bounding-polygon file=sk.poly --write-pbf sk_2024.pbf

# osmosis --read-pbf file=E:\planet-240115.osm.pbf --bounding-polygon file=yt.poly --write-pbf yt_2024.pbf
# osmosis --read-pbf file=E:\planet-240115.osm.pbf --bounding-polygon file=nvt.poly --write-pbf nvt_2024.pbf

# osmosis --read-pbf file=E:\planet-240115.osm.pbf --bounding-polygon file=nwt.poly --write-pbf nwt_2024.pbf

directory <- "C:/working/planet_osm/"



provinces <- data.frame(provinces) 
provinces$directory <- directory

provinces$filename <- paste0(directory, provinces$name, "_2024.pbf")
provinces$name <- paste0(provinces$name, "_2024")

geofabrik <- F
if(geofabrik){
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
  
  for(i in 1:nrow(provinces)){
    print(provinces$name[i])
    print(provinces$filename[i])
    download.file(url = paste0(source, provinces$filename[i]),
                  dest = paste0(national_data_local_directory, provinces$filename[i]),
                  method = "curl",
                  mode = "w")
  }
}

# load into postGIS
# this depends on having post gis installed, and a default account named 
# postgres with no password (when executed in a local environment). 
# also download, extract, and add to path osm2pgsql 
# https://osm2pgsql.org/doc/install.html#installing-on-windows
# and modifications to default.style
# Tested on Windows 10.

for(i in 1:nrow(provinces)){
  # load qc in 2018
  # provinces <- data.frame(provinces) 
  # provinces$directory <- directory
  # provinces <- provinces %>% filter(name %in% "qc_2024")
  # provinces[1, ]$name <- "qc_2018"
  # provinces[1, ]$filename <- "C:/working/planet_osm/quebec-180101.osm.pbf"
  # i <- 1
  
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
                " -S ", normalizePath(style_path), " ",
                paste0(provinces$filename[i])))
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
  print(paste0("Write to db ", provinces$name[i]))
  
}
