# Functions to classify OSM Can-BICS infrastructure type
library(lwgeom)
library(raster)
library(terra)
library(sf)
library(tidyverse)
source("./code/paths_and_variables.R")

# 1. Bicycle indicated
bikes_designated <- function(osm_dataframe){
  osm_dataframe$bicycle %in% c("designated")
}

bikes_suggested <- function(osm_dataframe){
  osm_dataframe$bicycle %in% c("yes", "permissive") 
}

bikes_restricted <- function(osm_dataframe){
  osm_dataframe$bicycle %in% c("no", "restricted", "dismount")
}

cycle_network <- function(osm_dataframe){
  osm_dataframe$local_cycle_network %in% T | 
    osm_dataframe$regional_cycle_network %in% T |
    osm_dataframe$national_cycle_network %in% T
}

on_street_facility <- function(osm_dataframe){
  ! is.na(osm_dataframe$cycleway) |
  ! is.na(osm_dataframe$cycleway.left) |
  ! is.na(osm_dataframe$cycleway.right) |
  ! is.na(osm_dataframe$cycleway.both)
}

bikes_indicated <- function(osm_dataframe){
  (osm_dataframe$highway %in% "cycleway" |
  bikes_designated(osm_dataframe) |
  bikes_suggested(osm_dataframe) |
  cycle_network(osm_dataframe) |
  on_street_facility(osm_dataframe)) & (! bikes_restricted(osm_dataframe))
}

#2. Road or path/trail
path <- function(osm_dataframe){
  osm_dataframe$highway %in% c("footway",
                               "pedestrian", 
                               "path", 
                               "bridleway", 
                               "cycleway",
                               "footway",
                               "track",
                               "raceway",
                               "service",
                               "crossing",
                               "sidewalk")
}

#3. Surface
paved <- function(osm_dataframe){
  osm_dataframe$surface %in% c("paved", "asphalt", "concrete", "concrete:lanes", 
                               "concrete:plates", "paving_stones", "sett", 
                               "unhewn_cobblestone", "cobblestone", 
                               "metal", "wood")
}

unpaved <- function(osm_dataframe){
  osm_dataframe$surface %in% c("unpaved", "compacted", "fine_gravel", "gravel", 
                               "ground", "dirt", "earth", "grass", "mud", 
                               "sand", "woodchips", "natural",
                               "loose_gravel", "unpaved;gravel") |
    ((! paved(osm_dataframe)) &
       (hike_mtb_indicated(osm_dataframe)))
  
}

hike_mtb_indicated <- function(osm_dataframe){
  (!is.na(osm_dataframe$mtb.scale.imba) | 
     !is.na(osm_dataframe$mtb.scale) |
     !is.na(osm_dataframe$mtb) |
     !is.na(osm_dataframe$sac_scale)) # hiking trail rating
}

# identify non-conforming trails based on attributes
non_conforming_trail_attributes <- function(osm_dataframe){
  unpaved(osm_dataframe) |
    hike_mtb_indicated(osm_dataframe) & ! paved(osm_dataframe) |
    osm_dataframe$highway %in% c("footway",
                                 "pedestrian",
                                 "sidewalk")  & !cycle_network(osm_dataframe) |
    osm_dataframe$highway %in% "service" & !cycle_network(osm_dataframe) 
}

# if surface is missing, check landcover
# source: https://open.canada.ca/data/en/dataset/4e615eae-b90c-420b-adee-2ca35896caf6
# https://ftp.maps.canada.ca/pub/nrcan_rncan/Land-cover_Couverture-du-sol/canada-landcover_canada-couverture-du-sol/CanadaLandcover2015.zip

# 17 is urban landcover
non_urban <- c(1, 2, 5, 6, 8, 10, 11, 12, 13, 14, 15, 16, 18, 19) 


# uses a (slow) loop to check extract landcover for each feature
# potential for improvement here if there is a more efficient method

# assumes there is a landcover raster called "landcover"
urban_landcover <- function(osm_dataframe){
  urban_result <- rep(NA, nrow(osm_dataframe))
  
  paths_check <- which(path(osm_dataframe) & 
                         is.na(osm_dataframe$surface) &
                         ! non_conforming_trail_attributes(osm_dataframe))
  
  print(paste0("checking ", length(paths_check), " features."))
  
  # if(debug){
  #   View(osm_dataframe[paths_check, ])
  # } 
  
  if(length(paths_check) > 0){
    for(i in 1:length(paths_check)){
      urban = NA
      print(i)
      print(i/length(paths_check))
      
      feature_index <- paths_check[i]
      selected_feature <- osm_dataframe[feature_index,]
      
      osm_id <- selected_feature$osm_id

      selected_highway <- osm_dataframe[paths_check[i], ]
      
      # check for match
      if(nrow(selected_highway) == 0){
        print(paste0("no matching OSM feature for osm_id: ", osm_id))
      } else if (cycle_network(selected_highway)) {
        print("bike route. assuming paved.")
        urban_result[feature_index] <- T
      } else {
        print(selected_highway$name)
        
        # if(debug){
        #   mapview(selected_highway)
        # }
        
        if(is.na(selected_highway$surface)){
          # check if located in an urban area
          selected_highway <- selected_highway %>% 
            st_transform(crs(landcover_terra))
          if(any(st_geometry_type(selected_highway) %in% "GEOMETRYCOLLECTION")){
            selected_highway <- st_collection_extract(
              x = selected_highway,
              type = c("LINESTRING"))
          }
          # lc <- raster::extract(landcover, selected_highway, buffer = 3)
          lc <- terra::extract(landcover_terra,
                               terra::vect(selected_highway))
          # DN = 17 is urban
          if(any(unlist(lc$CAN_LC_2015_CAL) %in% non_urban)){
            print("non-urban. assuming unpaved.")
            urban = F
            urban_result[feature_index] <- F
          } else {
            print("Urban. assuming paved.")
            urban_result[feature_index] <- T
          }
        }
      }
    }
  }
  return(urban_result)
}

###############################################################################
# path or road assignment

none <- function(osm_dataframe){
  !bikes_indicated(osm_dataframe)
}

non_conforming_trail <- function(osm_dataframe){
  non_conforming_trail_attributes(osm_dataframe) |
    urban_landcover(osm_dataframe) %in% F
}

can_path <- function(osm_dataframe){
  path(osm_dataframe) & 
    osm_dataframe$non_conforming_trail %in% F
}

road_or_path <- function(osm_dataframe){
  osm_dataframe$non_conforming_trail <- non_conforming_trail(osm_dataframe)
  
  osm_dataframe <- osm_dataframe %>% mutate(road_or_path = case_when(
    none(osm_dataframe) ~ "None",
    osm_dataframe$non_conforming_trail ~ "Non-Conforming Trail",
    can_path(osm_dataframe) ~ "Path",
    T ~ "Road"))
  
  return(osm_dataframe$road_or_path)
}

###############################################################################
# path type

foot_designated <- function(osm_dataframe){
  osm_dataframe$foot %in% c("designated")
}

foot_suggested <- function(osm_dataframe){
  osm_dataframe$foot %in% c("yes")
}

foot_indicated <- function(osm_dataframe){
  foot_designated(osm_dataframe) |
    foot_suggested(osm_dataframe) |
    osm_dataframe$highway %in% c("footway",
                                 "sidewalk") 
}

foot_restricted <- function(osm_dataframe){
  osm_dataframe$foot %in% c("no", "restricted")
}

bike_only <- function(osm_dataframe){
  bikes_indicated(osm_dataframe) & foot_restricted(osm_dataframe) |
    osm_dataframe$segregated %in% "yes"
}

multi_use <- function(osm_dataframe){
  foot_indicated(osm_dataframe) &
    ! foot_restricted(osm_dataframe) &
    ! osm_dataframe$segregated %in% "yes" 
}

###############################################################################
# cycle tracks and bike paths
################################################################################

bike_path_or_cycle_track_or_MUP <- function(osm_dataframe){
  bike_paths_test <- rep(NA, nrow(osm_dataframe))
  tracks <- which(osm_dataframe$road_or_path %in% "Path")
  
  if(length(tracks) == 0){
    return(bike_paths_test)
  }
  
  debug <- F
  
  # if(debug){
  #   View(osm_dataframe[tracks, ])
  # } 
  
  # first, MUPs based on attributes only
  mups <- which(osm_dataframe$road_or_path %in% "Path" &
                  multi_use(osm_dataframe))
  
  if(length(mups) > 0){
    bike_paths_test[mups] <- "Multi-Use Path"
    
    tracks <- tracks[-which(tracks %in% mups)]
  }
  
  # there is one other case for MUPS: bike paths with no ped
  # restrictions: addressed below
  
  # cycletracks and bike paths
  if(length(tracks) == 0){
    return(bike_paths_test)
  }
  
  for(i in 1:length(tracks)){
    print(i)
    print(i/length(tracks))
    
    feature_index <- tracks[i]
    selected_feature <- osm_dataframe[feature_index,]
    
    osm_id <- selected_feature$osm_id

    selected_highway <- osm_dataframe[which(osm_dataframe$osm_id %in%
                                              osm_id), ]

    # a single highway could be split into two or more pieces, if crossing a 
    # census line. In that case, you only need one because they are identical,
    # (other than the census id)
    if(nrow(selected_highway) > 1){
      selected_highway <- selected_highway[-which(duplicated(selected_highway$osm_id)), ]
    }
        
    selected_highway$length <- selected_highway %>% st_length() %>% as.numeric()
    
    print(selected_highway$name)
    
    # if(debug){
    #   mapview(selected_highway)
    # }
    
    search_dist <- 15
    
    # search for highways within 10 m
    segments_search_dist <- osm_dataframe %>%
      st_intersection(st_buffer(selected_highway, search_dist)) %>%
      filter(! osm_id %in% selected_highway$osm_id)
    
    segments_search_dist$length <- st_length(segments_search_dist) %>%
      as.numeric()
    
    # if(debug){
    #   mapview(segments_search_dist["length"])
    # }
    
    # select roads
    road_types <- c(
      "trunk",
      "motorway",
      "primary",
      "secondary",
      "tertiary",
      "unclassified",
      "residential",
      "motorway_link",
      "trunk_link",
      "primary_link",
      "secondary_link",
      "tertiary_link",
      "living_street")
    
    segments_search_dist_roads <- segments_search_dist %>%
      filter(highway %in% road_types) 
    
    # select footways
    segments_search_dist_footways <- segments_search_dist %>%
      filter(highway %in% c(
        "footway",
        "pedestrian",
        "sidewalk")) 
    
    # find parallel streets - at least 40 m, and at least 30% of total length
    if(nrow(segments_search_dist_roads) > 0){      
      parallel <- which(segments_search_dist_roads$length > 40)
      parallel_roads <- segments_search_dist_roads[parallel, ]
      # if(debug){
      #   mapview(parallel_roads["length"])
      # }
      
      parallel_road <- nrow(parallel_roads) > 0
      
      # filter out the situation where the parallel road is a small proportion
      if(sum(parallel_roads$length) < (0.3 * selected_highway$length)){
        parallel_road <- F
      } 
      
      # the other case is that a short segment is surrounded by roads
      if(sum(segments_search_dist_roads$length) > 4 * selected_highway$length){
        parallel_road <- T
      }
      
    } else {
      parallel_road <- F
    }
    
    # find parallel footways - at least 40 m
    if(nrow(segments_search_dist_footways) > 0){
      parallel <- which(segments_search_dist_footways$length > 40)
      parallel_footways <- segments_search_dist_footways[parallel, ]
      # if(debug){
      #   #mapview(parallel_footways["length"])
      # }
      parallel_footway <- nrow(parallel_footways) > 0
    } else {
      parallel_footway <- F
    }
    
    if(! parallel_road){
      # burden of proof: must actually say that pedestrians are restricted or 
      # separated, otherwise, MUP
      
      if(bike_only(selected_feature)){
        print("Bike Path")
        bike_paths_test[feature_index] <- "Bike Path"
      } else {
        print("Multi-Use Path")
        bike_paths_test[feature_index] <- "Multi-Use Path"
      }
    } else {
      print("Cycle Track")
      bike_paths_test[feature_index] <- "Cycle Track"
    }
  }
  
  return(bike_paths_test)
}

################################################################################
# Roadways
################################################################################

cycletrack_on_roadway <- function(osm_dataframe){
  osm_dataframe$road_or_path %in% "Road" &
    (osm_dataframe$cycleway %in% "track" |
       osm_dataframe$cycleway.both %in% "track" |
       osm_dataframe$cycleway.left %in% "track" |
       osm_dataframe$cycleway.right %in% "track")
}

painted_lane <- function(osm_dataframe){
  osm_dataframe$road_or_path %in% "Road" &
    (osm_dataframe$cycleway %in% c("exclusive", 
                                   "lane", 
                                   "buffered_lane") | 
       osm_dataframe$cycleway.left %in% c("exclusive", 
                                          "lane", 
                                          "buffered_lane") |
       osm_dataframe$cycleway.right %in% c("exclusive", 
                                           "lane", 
                                           "buffered_lane") |
       osm_dataframe$cycleway.both %in% c("exclusive", 
                                          "lane", 
                                          "buffered_lane") |
       (osm_dataframe$cycleway.left %in% "opposite_lane" &
          osm_dataframe$cycleway.right %in% "shared_lane") |
       (osm_dataframe$cycleway.right %in% "opposite_lane" &
          osm_dataframe$cycleway.left %in% "shared_lane"))
}

# local street bikeway
# road type
major_street <- function(osm_dataframe){
  # residential street with a reduced speed limit
  (osm_dataframe$highway %in% c("motorway",
                                "motorway_link", 
                                "trunk",
                                "trunk_link", 
                                "primary",
                                "primary_link",
                                "secondary",
                                "secondary_link",
                                "tertiary",
                                "tertiary_link") |
     speed_not_limited(osm_dataframe))
}

minor_road <- function(osm_dataframe){
  osm_dataframe$highway %in% c("residential", 
                               "unclassified", 
                               "living_street",
                               "track")
}

speed_limited <- function(osm_dataframe){
  # character field
  osm_dataframe$maxspeed %in% c("5", "10", "15", "20", "25", "30")
}

speed_not_limited <- function(osm_dataframe){
  # character field
  osm_dataframe$maxspeed %in% c(
    "40", "45",
    "50", "55", "60", "65",
    "70", "75", "80", "85",
    "90", "95", "100", "105",
    "110", "115", "120", "125",
    "130", "135", "140") 
}

local_street <- function(osm_dataframe){
  # minor paved road with a reduced speed limit
  (minor_road(osm_dataframe) & ! speed_not_limited(osm_dataframe) &
     ! unpaved(osm_dataframe))
}

# road characteristics
traffic_calming_attributes <- function(osm_dataframe){
  speed_limited(osm_dataframe) |
    osm_dataframe$traffic_calming %in% "yes" 
}

traffic_diversion_attributes <- function(osm_dataframe){
  # one way for motor vehicles, but not bikes
  osm_dataframe$oneway %in% T & osm_dataframe$oneway.bicycle %in% F |
    (osm_dataframe$access %in% "no" & # no motorized access
       osm_dataframe$hgv %in% c("designated"))  # heavy goods vehicle e.g. bus lane)
}

bikeway <- function(osm_dataframe){
  osm_dataframe$cycleway %in% c("shared", "designated", "shared_lane") |
    osm_dataframe$cycleway.left %in% c("shared", "designated", "shared_lane") |
    osm_dataframe$cycleway.right %in% c("shared", "designated", "shared_lane") |
    bikes_designated(osm_dataframe) | 
    cycle_network(osm_dataframe)
}

local_street_bikeway_candidate <- function(osm_dataframe){
  local_street(osm_dataframe) & 
    bikeway(osm_dataframe)
}

local_street_bikeway_attributes <- function(osm_dataframe){
  local_street_bikeway_candidate(osm_dataframe) & 
    (traffic_calming_attributes(osm_dataframe) |
       traffic_diversion_attributes(osm_dataframe) |
       speed_limited(osm_dataframe))
}


###############################################################################
# geometry check for local street bikeways
# assumes that paths have already been classified

local_street_bikeway_geometry <- function(osm_dataframe){
  
  lsbs <- which(osm_dataframe %>% local_street_bikeway_candidate & 
                  !local_street_bikeway_attributes(osm_dataframe))
  
  lsb_geometry <- rep(NA, nrow(osm_dataframe))
  
  debug <- F
  
  # if(debug){
  #   View(osm_dataframe[lsbs, ])
  # }  
  
  if(length(lsbs > 0)){
    for(i in 1:length(lsbs)){
      print(i)
      
      print(i/length(lsbs))
      
      feature_index <- lsbs[i]
      selected_feature <- osm_dataframe[feature_index,]
      
      osm_id <- selected_feature$osm_id

      selected_highway <- osm_dataframe[which(osm_dataframe$osm_id %in% 
                                                osm_id), ]
      
      # a single highway could be split into two or more pieces, if crossing a 
      # census line. In that case, you only need one because they are identical,
      # (other than the census id)
      if(nrow(selected_highway) > 1){
        selected_highway <- selected_highway[-which(duplicated(selected_highway$osm_id)), ]
      }
      
      print(selected_highway$name)
      
      # if(debug){
      #   mapview(selected_highway)
      # }
      
      search_dist <- 750
      
      # find segments with the same name
      # search on the same route, or if there's no route, on the same road.
      
      route <- (! is.na(selected_highway$local_cycle_network_name) |
                  ! is.na(selected_highway$regional_cycle_network_name) |
                  ! is.na(selected_highway$national_cycle_network_name))
      
      if(route){
        segments_same_name <- selected_highway
        
        if(! is.na(selected_highway$local_cycle_network_name)){
          segments_same_local_cycle_network_name <- osm_dataframe %>%
            # filter(CSDNAME %in% selected_highway$CSDNAME) %>%
            filter(local_cycle_network_name %in% 
                     selected_highway$local_cycle_network_name)
        } else {
          segments_same_local_cycle_network_name <- NULL
        }
        
        if(! is.na(selected_highway$regional_cycle_network_name)){
          segments_same_regional_cycle_network_name <- osm_dataframe %>%
            # filter(CSDNAME %in% selected_highway$CSDNAME) %>%
            filter(regional_cycle_network_name %in% 
                     selected_highway$regional_cycle_network_name)
        } else {
          segments_same_regional_cycle_network_name <- NULL
        }
        
        if(! is.na(selected_highway$national_cycle_network_name)){
          segments_same_national_cycle_network_name <- osm_dataframe %>%
            # filter(CSDNAME %in% selected_highway$CSDNAME) %>%
            filter(national_cycle_network_name %in% 
                     selected_highway$national_cycle_network_name)
        } else {
          segments_same_national_cycle_network_name <- NULL
        }
      } else {
        if(! is.na(selected_highway$name)){
          segments_same_name <- osm_dataframe %>%
            # filter(CSDNAME %in% selected_highway$CSDNAME) %>%
            filter(name %in% selected_highway$name)
        } else {
          segments_same_name <- selected_highway
        }
        segments_same_local_cycle_network_name <- NULL
        segments_same_regional_cycle_network_name <- NULL
        segments_same_national_cycle_network_name <- NULL
      }
      
      segments_same_name <- rbind(segments_same_name,
                                  segments_same_local_cycle_network_name,
                                  segments_same_regional_cycle_network_name,
                                  segments_same_national_cycle_network_name) %>%
        filter(! (path(segments_same_name) | unpaved(segments_same_name)))
      
      segments_same_name_search_dist <- segments_same_name %>%
        st_intersection(st_buffer(selected_highway, search_dist))
      
      # if(debug){
      #   mapview(segments_same_name_search_dist)
      # }
      
      # first check for roundabouts - fastest
      
      # select roundabouts that are touching
      touching_threshold <- 5 # if within 5 m
      touching_segments <- osm_dataframe %>% 
        # filter(osm_dataframe$CSDNAME %in% selected_highway$CSDNAME) %>%
        st_intersection(segments_same_name_search_dist %>%
                          st_buffer(touching_threshold))
      
      touching_roundabouts <- roundabouts %>% 
        st_intersection(segments_same_name_search_dist %>%
                          st_buffer(touching_threshold))
      
      if(nrow(touching_roundabouts) > 0){
        # if(debug){
        #   mapview(touching_roundabouts)
        # }
      }
      # if(debug){
      #   mapview(rbind(touching_segments %>% dplyr::select(osm_id),
      #                 touching_roundabouts %>% dplyr::select(osm_id)))
      # }
      
      if(T %in% (touching_segments$junction %in%
                 "roundabout") |
         nrow(touching_roundabouts) > 0){
        lsb_geometry[feature_index] <- "Roundabout"
        print("Roundabout!")
      } else {
        # check for dead ends
        # find end points for the search area
        
        # 1. Get the full extent of the highways
        highways_extended <- osm_dataframe[which(osm_dataframe$osm_id %in% 
                                                   touching_segments$osm_id), ]
        
        # if(debug){
        #   mapview(highways_extended["osm_id"])
        #   
        # }
        
        start_points <- highways_extended %>%
          st_startpoint() %>%
          st_as_sf()
        end_points <- highways_extended %>%
          st_endpoint() %>%
          st_as_sf()
        start_end_points <- rbind(start_points,
                                  end_points)
        
        # if(debug){
        #   mapview(start_end_points)
        # }
        
        # find endpoints that do not continue
        
        # find roads touching along the same route
        touching_dist <- 1
        start_end_points$road_count <- lengths(st_intersects(st_buffer(start_end_points, touching_dist), 
                                                             start_end_points)) 
        start_end_points_route <- start_end_points %>%
          st_intersection(st_buffer(segments_same_name_search_dist, touching_dist))
        
        # if(debug){
        #   mapview(start_end_points_route["road_count"])
        # }
        
        if(length(which(start_end_points_route$road_count == 3)) > 0){
          deadends <- start_end_points_route %>%
            filter(road_count == 3)
          # find paths touching
          # if(debug){
          #   mapview(deadends)
          # }
          touching_threshold <- 5
          
          deadend_touching_path_track_segments <- osm_dataframe %>% 
            st_intersection(deadends %>%
                              st_buffer(touching_threshold)) %>%
            filter(path_type %in% c("Cycle Track",
                                    "Multi-Use Path",
                                    "Bike Path"))
          if(nrow(deadend_touching_path_track_segments) >0) {
            # if(debug){
            #   mapview(deadend_touching_path_track_segments)
            # }
          }
          
          if(nrow(deadend_touching_path_track_segments) > 0){
            lsb_geometry[feature_index] <- "Traffic diversion"
            print("Traffic diversion!")
          }
        } 
      }
    }    
  }
  return(lsb_geometry)
}

################################################################################
# classify
################################################################################
unmatched <- function(osm_dataframe){
  osm_dataframe$nn_dist > 20
}

classify_training_data <- function(training,
                                   highways){
  training_join <- training %>%
    left_join(highways %>% 
                st_drop_geometry() %>%
                dplyr::select(osm_id,
                              road_or_path,
                              path_type,
                              lsb_geom,
                              Can_BICS), 
              by = c("nn_osm_id" = "osm_id"))
  return(training_join)
}

classify_highways <- function(highways,
                              CSDS,
                              roundabouts){
  
  highways_csds <- highways %>%
    st_intersection(CSDS)
  
  highways_predicted <- highways_csds %>% 
    important_tags(c("CSDNAME", 
                     "CSDUID",
                     "PRNAME",
                     desired_osm_columns))
  
  highways_predicted$road_or_path <- road_or_path(highways_predicted)
  highways_predicted$path_type <- bike_path_or_cycle_track_or_MUP(highways_predicted)
  highways_predicted$lsb_geom <- local_street_bikeway_geometry(highways_predicted)
  highways_predicted <- highways_predicted %>% 
    mutate(Can_BICS = case_when(
      road_or_path %in% c("None") ~ "None",
      road_or_path %in% c("Non-Conforming Trail") ~ "Non-Conforming Trail",
      !is.na(path_type) ~ paste0(path_type),
      painted_lane(highways_predicted) ~ "Painted Bike Lane",
      cycletrack_on_roadway(highways_predicted) ~ "Cycle Track",
      ! is.na(highways_predicted$lsb_geom) | 
        local_street_bikeway_attributes(highways_predicted) ~ "Local Street Bikeway",
      bikeway(highways_predicted) & 
        major_street(highways_predicted) ~ "Non-Conforming Major Road",
      T ~ "Non-Conforming Other"
    ))
  return(highways_predicted)
}
