# the purpose of this script is to automate some data checks and export in a
# format that works for ArcGIS online

library(tidyverse)
library(sf)
library(RPostgreSQL)
source("./code/Can_BICS_OSM_functions.R")

central_db_name <- "Can_BICS_provinces"

# load highways from db
try(conn <- dbConnect(PostgreSQL(), 
                      dbname = central_db_name,
                      user = "postgres"))
tables <- dbListTables(conn)
tables_highways_predicted <- tables[str_detect(tables, 
                                               "highways_predicted_[:alpha:]+$")]
dbDisconnect(conn)

highways <- data.frame()

# for qc in 2018
tables_highways_predicted <- "highways_predicted_qc_2018_2018"

# select desired fields
desired_columns <- c(desired_osm_columns,
                     "CSDNAME",
                     "CSDUID",
                     "PRNAME",
                     "PRUID",
                     "Can_BICS",
                     "Can_BICS_class")

for(i in 1:length(tables_highways_predicted)){
  name <- tables_highways_predicted[i]
  print(name)
  prov_name <- name %>%
    str_extract("[^\\_]+$")
  
  try(conn <- dbConnect(PostgreSQL(), 
                        dbname = central_db_name,
                        user = "postgres"))
  do.call("<-",list("new_highways",
                    st_read(dsn = conn,
                            layer = tables_highways_predicted[i])))
  
  new_highways <- new_highways %>%
    important_tags(desired_columns)
  
  dbDisconnect(conn)
  
  if(nrow(highways) ==0){
    highways <- new_highways
  } else {
    highways <- rbind(highways, new_highways)
  }
  
  
}

# clean up
highways_output <- highways %>%
  mutate(Can_BICS = recode(Can_BICS,
                           "None" = "NA",
                           "Cycle Track (on roadway)" = "Cycle Track",
                           "Painted Lane" = "Painted Bike Lane",
                           "Non-standard major road" = "Non-conforming major road",
                           "Non-standard other" = "Non-conforming other",
                           "Non-standard trail" = "Non-conforming trail")) %>%
  mutate(Can_BICS_class = Can_BICS_class(Can_BICS)) %>%
  mutate(Can_BICS_class = recode(Can_BICS_class,
                                 "None" = "NA")) %>%
  mutate(Can_BICS = na_if(Can_BICS,
                          "NA"),
         Can_BICS_class = na_if(Can_BICS_class,
                                "NA"))

# check 
table(highways_output$Can_BICS)
table(highways_output$Can_BICS,
      highways_output$Can_BICS_class)

# write to database for checking
try(conn <- dbConnect(PostgreSQL(), 
                      dbname = central_db_name,
                      user = "postgres"))


st_write(highways_output,  
         dsn = conn, 
         layer = "highways_output",
         delete_dsn = T)

dbDisconnect(conn)

# # write to geojson for uploading
st_write(highways_output %>%
           filter(! is.na(Can_BICS)),
           "data/national/highways_output_v3.geojson",
           delete_dsn = T)
# 
# # write to gpkg for conversion and display
# st_write((highways_output %>%
#            filter(! is.na(Can_BICS)) %>%
#             dplyr::select(CSDNAME, PRNAME, osm_id, name, Can_BICS, Can_BICS_class)),
#          "data/provincial_data/highways_output.gpkg",
#          delete_dsn = T)

# write to shp for sharing
# shorten names for shapefile see https://github.com/r-spatial/sf/issues/1206
# less than 10 characters

st_write((highways_output %>%
           filter(! is.na(Can_BICS)) %>%
           dplyr::select(CSDNAME, 
                         CSDUID,
                         PRNAME, 
                         PRUID,
                         osm_id, 
                         osm_name = name, 
                         CBICS_infr = Can_BICS, 
                         CBICS_comf = Can_BICS_class) %>%
            st_collection_extract("LINESTRING")),
            "data/national/OSM_CAN_BICS_qc_2018.shp",
           overwrite = T,
           delete_dsn = T)


# write intrastructure type and comfort class descriptions
metadata_infra <- data.frame(`Comfort Class` = c("High", 
                                    "High",
                                    "High", 
                                    "Medium",
                                    "Low",
                                    "Non-Conforming",
                                    "Non-Conforming",
                                    "Non-Conforming"),
                       `Infrastructure Type` = c("Bike Path", 
                                       "Cycletrack",
                                       "Local Street Bikeway", 
                                       "Multi-Use Path",
                                       "Painted Bike Lane",
                                       "Non-Conforming Trail",
                                       "Non-Conforming Major Road",
                                       "Non-Conforming Other"),
                       `Description` = c("A paved path exclusively for cyclists away from roadways",
                                         "A roadway lane exclusively for cyclists and physically separated from both motor vehicles and the sidewalk.",
                                         "A local street (no centre line or lanes) where cyclists share the roadway with motor vehicles. Traffic-calming elements limit motor vehicle speeds and volumes and inhibit their through travel.",
                                         "A two-way paved path shared by cyclists, pedestrians and other users. May be located adjacent to a roadway, if physically segregated from cars and trucks.",
                                         "A painted lane along a roadway that is designated by bicycle and diamond pavement markings and signs as exclusively for cyclists, with no physical segregation from cars and trucks.",
                                         "An unpaved or narrow (<3 m) path shared by cyclists, pedestrians and other users. Includes hiking and mountain biking trails, desire paths, and sidewalks where bike are permitted.",
                                         "A major street (multiple lanes) where cyclists share the roadway with motor vehicles (i.e., sharrows).",
                                         "All other segments where bikes are indicated on OSM without sufficient information to assign a more specific label."))

# write field descriptions
metadata <- data.frame(Variable = c("CSDNAME", 
                                    "CSDUID",
                                    "PRNAME", 
                                    "PRUID",
                                    "osm_id", 
                                    "osm_name", 
                                    "CBICS_infr", 
                                    "CBICS_comfs"),
                      Description = c("Statistics Canada Census subdivision name.",
                                      "Statistics Canada Census subdivision ID.",
                                      "Statistics Canada Census province name.",
                                      "Statistics Canada Census province ID",
                                      "OpenStreetMap ID. Note that these can change over time.",
                                      "OpenStreetMap name.",
                                      "Can-BICS infrastructure type.",
                                      "Can-BICS comfort class."))

library(knitr)
kable(metadata_infra)

kable(metadata)
