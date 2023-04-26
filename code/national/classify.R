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

library(RPostgreSQL)

lookup_pruid <- function(province_abrev){
  case_when(
    province_abrev %in% "ab" ~ 48,
    province_abrev %in% "bc" ~ 59,
    province_abrev %in% "mb" ~ 46,
    province_abrev %in% "nb" ~ 13,
    province_abrev %in% "nf" ~ 10,
    province_abrev %in% "nwt" ~ 61,
    province_abrev %in% "ns" ~ 12,
    province_abrev %in% "nvt" ~ 62,
    province_abrev %in% "on" ~ 35,
    province_abrev %in% "pei" ~ 11,
    province_abrev %in% "on" ~ 35,
    province_abrev %in% "qc" ~ 24,
    province_abrev %in% "sk" ~ 47,
    province_abrev %in% "yt" ~ 60
  )
}

# load highways from db
try(conn <- dbConnect(PostgreSQL(), 
                      dbname = central_db_name,
                      user = "postgres"))
  tables <- dbListTables(conn)
  tables_highways <- tables[str_detect(tables, "highways_[:alpha:]+$")]
  tables_roudabouts <- tables[grep("round*", tables)]
dbDisconnect(conn)


  if(length(tables_highways) != length(tables_roudabouts)){
    print("warning: missing highways or roundabouts")
  }


# break into pieces
tables_highways <- tables_highways[order(tables_highways)]
tables_roudabouts <- tables_roudabouts[order(tables_roudabouts)]

test <- grep("nb",tables_highways)
western <- grep("bc", tables_highways)
prairies <- grep("ab|mb|sk", tables_highways)
ontario <- grep("on", tables_highways)
quebec <- grep("qc", tables_highways)
eastcoast <- grep("nb|pei|nf|ns", tables_highways)
northern <- grep("nvt|nwt|yt", tables_highways)

subset_data <- T
subset_select <- c(northern)

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
                                layer = "roundabouts_yt")))
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

