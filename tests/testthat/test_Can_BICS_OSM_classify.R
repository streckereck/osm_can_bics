library(sf)
library(tidyverse)
library(testthat)

local_edition(3)

# load sample data from Vancouver
selected_csds <- st_read("data/test_cities/selected_csds.gpkg") %>%
  filter(CSDNAME %in% "Vancouver")
highways_select <- st_read("data/test_cities/highways_all.gpkg") %>%
  st_intersection(selected_csds)
roundabouts <- st_read("data/test_cities/roundabouts.gpkg") %>%
  st_intersection(selected_csds)

source("./code/Can_BICS_OSM_classify.R")
source("./code/Can_BICS_OSM_functions.R")

# Carrall street - cycletrack
carrall <- highways_select %>%
  filter(osm_id %in% 872769249)

# Beach ave - cycletrack
beach <- highways_select %>%
  filter(osm_id %in% 863819776)

# 7th - local street bikeway
seventh_street <- highways_select %>%
  filter(osm_id %in% 309518515)

# Trans Canada Trail - non conforming trail
tct <- highways_select %>%
  filter(osm_id %in% 71493594)

# Arbutus greenway
arbutus <- highways_select %>%
  filter(osm_id %in% 336803184)

# 7th cycletrack
seventh_cycle <- highways_select %>%
  filter(osm_id %in% 511798517)

test_that("road_or_path", {
  expect_equal(road_or_path(carrall), "Path")
  expect_equal(road_or_path(beach), "Path")
  expect_equal(road_or_path(seventh_street), "Road")
  expect_equal(road_or_path(tct), "Non-Conforming Trail")
  expect_equal(road_or_path(arbutus), "Path")
  expect_equal(road_or_path(seventh_cycle), "Path")
  
})

# bike ottawa osm coding guidelines: 
# https://github.com/BikeOttawa/OSM-Bike-Ottawa-Tagging-Guide

## the following should be based on attributes alone
# just pick any geometry and assign tags for testing
geom <- seventh_cycle
geom[ , 1:(length(geom) - 3)] <- NA # erase all attributes

# paved mup
paved_mup <- geom
paved_mup <- paved_mup %>%
  mutate(name = "Paved Multi-Use Path (MUP)",
         highway = "path",
         bicycle = "yes",
         segregated = "no",
         surface = "asphalt",
         Can_BICS_ground = "Multi-Use Path")

bike_path <- geom
bike_path <- bike_path %>%
  mutate(name = "Twinned Path",
         bicycle = "yes",
         highway = "path",
         segregated = "yes",
         surface = "asphalt",
         Can_BICS_ground = "Bike Path")

walkway <- geom
walkway <- walkway %>%
  mutate(name = "Walkway",
         highway = "footway",
         bicycle = "yes",
         segregated = "no",
         surface = "asphalt",
         Can_BICS_ground = "Non-conforming path")

unpaved_mup <- geom
unpaved_mup <- unpaved_mup %>%
  mutate(name = "Unpaved MUP",
         highway = "path",
         bicycle = "yes",
         segregated = "no",
         surface = "fine_gravel",
         Can_BICS_ground = "Non-conforming path")

desire <- geom
desire <- desire %>%
  mutate(name = "Desire line",
         highway = "path",
         path = "desire",
         Can_BICS_ground = "Non-conforming path")

## protected lanes
# need to select a suitable geometry - road + cycle track
# 360217193 - Comox cycletrack
# 221532057, 476612027, 476612031, 476610618, 30735293 - Comox street

protected <- highways_select %>%
  filter(osm_id %in% c(360217193,
                       221532057,
                       476612027,
                       476612031,
                       476610618,
                       30735293
                       ))

protected <- classify_highways(protected,
                               csds,
                               roundabouts) %>%
  filter(osm_id %in% 360217193)


# painted lanes
painted <- geom
painted <- painted %>%
  mutate(name = "Painted Bike Lane",
         highway = "tertiary",
         cycleway = "lane",
         Can_BICS_ground = "Painted Bike Lane")


test_that("bike_ottawa_guidelines", {
  suppressWarnings(
    expect_equal(classify_highways(paved_mup,
                                   csds,
                                   roundabouts) %>%
                   st_drop_geometry() %>%
                   dplyr::select(Can_BICS) %>%
                   as.character(), 
                 "Multi-Use Path"))
  suppressWarnings(  
    expect_equal(classify_highways(bike_path,
                                   csds,
                                   roundabouts) %>%
                   st_drop_geometry() %>%
                   dplyr::select(Can_BICS) %>%
                   as.character(), 
                 "Bike Path"))
  suppressWarnings(
    expect_equal(classify_highways(walkway,
                                   csds,
                                   roundabouts) %>%
                   st_drop_geometry() %>%
                   dplyr::select(Can_BICS) %>%
                   as.character(), 
                 "Non-Conforming Trail"))
  suppressWarnings(
    expect_equal(classify_highways(unpaved_mup,
                                   csds,
                                   roundabouts) %>%
                   st_drop_geometry() %>%
                   dplyr::select(Can_BICS) %>%
                   as.character(), 
                 "Non-Conforming Trail"))
  suppressWarnings(
    expect_equal(classify_highways(desire,
                                   csds,
                                   roundabouts) %>%
                   st_drop_geometry() %>%
                   dplyr::select(Can_BICS) %>%
                   as.character(), 
                 "None"))
  suppressWarnings(
    expect_equal(protected %>%
                   st_drop_geometry() %>%
                   dplyr::select(Can_BICS) %>%
                   as.character(), 
                 "Cycle Track"))
  suppressWarnings(
    expect_equal(classify_highways(painted,
                                   csds,
                                   roundabouts) %>%
                   st_drop_geometry() %>%
                   dplyr::select(Can_BICS) %>%
                   as.character(), 
                 "Painted Bike Lane"))
  suppressWarnings(
    expect_equal(classify_highways(painted,
                                   csds,
                                   roundabouts) %>%
                   st_drop_geometry() %>%
                   dplyr::select(Can_BICS) %>%
                   as.character(), 
                 "Painted Bike Lane")
  )
})
