# The goal of this script is a preliminary classification based on semantic
# interpretation of tags.
# 
# 1. Load data

library(caret)
library(janitor)
library(lwgeom)
library(mapview)
library(sf)
library(tidyverse)
source("./code/Can_BICS_OSM_classify.R")
source("./code/Can_BICS_OSM_functions.R")

central_db_name <- "Can_BICS_provinces"

# update path to CSD boundaries
csds <- st_read("C:/Users/16043/Documents/basemap/census_2016/census_subdivision_boundary/lcsd000a16a_e.shp")

library(RPostgreSQL)

lookup_pruid <- function(province_abrev){
  case_when(
    province_abrev %in% "AB" ~ 48,
    province_abrev %in% "BC" ~ 59,
    province_abrev %in% "MB" ~ 46,
    province_abrev %in% "NB" ~ 13,
    province_abrev %in% "NF" ~ 10,
    province_abrev %in% "NWT" ~ 61,
    province_abrev %in% "NS" ~ 12,
    province_abrev %in% "NV" ~ 62,
    province_abrev %in% "ON" ~ 35,
    province_abrev %in% "PEI" ~ 11,
    province_abrev %in% "ON" ~ 35,
    province_abrev %in% "QC" ~ 24,
    province_abrev %in% "SK" ~ 47,
    province_abrev %in% "YT" ~ 60
  )
}

# load highways from db
try(conn <- dbConnect(PostgreSQL(), 
                      dbname = central_db_name,
                      user = "postgres"))
  tables <- dbListTables(conn)
  tables_highways <- tables[str_detect(tables, "highways_[:alpha:]+$")]
  tables_highways <- tables_highways[-which(tables_highways %in% "highways_output")]
  tables_roudabouts <- tables[grep("round*", tables)]
dbDisconnect(conn)


  if(length(tables_highways) != length(tables_roudabouts)){
    print("warning: missing highways or roundabouts")
  }


# break into pieces
tables_highways <- tables_highways[order(tables_highways)]
tables_roudabouts <- tables_roudabouts[order(tables_roudabouts)]

western <- grep("BC",tables_highways)
prairies <- grep("AB|MB|SK",tables_highways)
ontario <- grep("ON",tables_highways)
quebec <- grep("QC",tables_highways)
eastcoast <- grep("NB|PEI|NF|NS",tables_highways)
northern <- grep("NV|NWT|YT",tables_highways)

subset_data <- T
subset_select <- c(eastcoast, northern)

if(subset_data){
  tables_highways <- tables_highways[subset_select]
  tables_roudabouts <- tables_roudabouts[subset_select]
}
  
  for(i in 1:length(tables_highways)){
    name <- tables_highways[i]
    print(name)
    prov_name <- name %>%
      str_extract("[^\\_]+$")
    
    try(conn <- dbConnect(PostgreSQL(), 
                          dbname = central_db_name,
                          user = "postgres"))
      do.call("<-",list("highways",
                        st_read(dsn = conn,
                                layer = tables_highways[i])))
      
      do.call("<-",list("roundabouts",
                      st_read(dsn = conn,
                              layer = tables_roudabouts[i])))
    dbDisconnect(conn)
      
  # classify
    selected_csds <- csds %>%
      filter(PRUID %in% lookup_pruid(prov_name)) %>%
      st_make_valid()
    
  # check for empty data, if empty, use all of Canada
  if(nrow(selected_csds) == 0){
    selected_csds <- csds %>%
      st_make_valid()
  }
    
  # check for empty data, if empty, use an empty dataframe
    if(nrow(roundabouts) == 0){
      try(conn <- dbConnect(PostgreSQL(), 
                            dbname = central_db_name,
                            user = "postgres"))
      do.call("<-",list("roundabouts",
                        st_read(dsn = conn,
                                layer = "roundabouts_YT")))
      dbDisconnect(conn)
      
      roundabouts <- roundabouts[-c(1:nrow(roundabouts)),]
      
    }
    
    highways_predicted <- classify_highways(highways = highways,
                                            CSDS = selected_csds,
                                            roundabouts = roundabouts)
    
  # write to DB
    try(conn <- dbConnect(PostgreSQL(), 
                          dbname = central_db_name,
                          user = "postgres"))
      st_write(highways_predicted,  
             dsn = conn, 
             layer = paste0("highways_predicted_", prov_name),
             delete_dsn = T)
    dbDisconnect(conn)
  }

