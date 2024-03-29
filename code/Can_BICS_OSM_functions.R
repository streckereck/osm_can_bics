################################################################################
# Assign class using infrastructure type
################################################################################

Can_BICS_class <- function(can_BICS){
  case_when(
    can_BICS %in% c("Painted Bike Lane") ~ "3. Low Comfort",
    can_BICS %in% c("Multi-Use Path") ~ "2. Medium Comfort",
    can_BICS %in% c("Cycle Track") ~ "1. High Comfort",
    can_BICS %in% c("Bike Path") ~ "1. High Comfort",
    can_BICS %in% c("Local Street Bikeway") ~ "1. High Comfort",
    can_BICS %in% c("Non-standard trail") ~ "Non-Conforming",
    can_BICS %in% c("Non-standard major road") ~ "Non-Conforming",
    can_BICS %in% c("Non-standard other") ~ "Non-Conforming",
    can_BICS %in% c("Non-conforming trail") ~ "Non-Conforming",
    can_BICS %in% c("Non-conforming major road") ~ "Non-Conforming",
    can_BICS %in% c("Non-conforming other") ~ "Non-Conforming",
    can_BICS %in% c("Non-Conforming Trail") ~ "Non-Conforming",
    can_BICS %in% c("Non-Conforming Major Road") ~ "Non-Conforming",
    can_BICS %in% c("Non-Conforming Other") ~ "Non-Conforming",
    can_BICS %in% c("Non-Conforming") ~ "Non-Conforming",
    can_BICS %in% c("None") ~ "None",
    TRUE ~ "None")
}

################################################################################
# Download highways and bike routes for a place
################################################################################

get_osm_highways <- function(city_name = character()){
  highways <- getbb(city_name) %>%
    opq()%>%
    add_osm_feature(key = "highway") %>%
    osmdata_sf(quiet = F,
               stringsAsFactors = F)
  
  highways_lines <- highways$osm_lines %>%
    st_transform(target_projection)
  return(highways_lines)
}

get_osm_bike_routes <- function(city_name = character()){
  getbb(city_name) %>%
    opq()%>%
    add_osm_feature(key = "route",
                    value = "bicycle") %>%
    osmdata_sf(quiet = F,
               stringsAsFactors = F)
}

get_osm_turn_restrictions <- function(city_name = character()){
  turn_restrictions <- getbb(city_name) %>%
    opq()%>%
    add_osm_feature(key = "type",
                    value = "restriction") %>%
    osmdata_sf(quiet = F,
               stringsAsFactors = F) 
  if(nrow(turn_restrictions$osm_lines) > 0){
    turn_restrictions_lines <-  turn_restrictions$osm_lines[,c("osm_id", 
                                                               "name")]
  } else {
    turn_restrictions_lines <-  data.frame("osm_id" = integer(), 
                                           "name" = character()) %>%
      st_as_sf()
  }

  return(turn_restrictions_lines)
}

get_osm_roundabouts <- function(city_name = character()){
  roundabouts <- getbb(city_name) %>%
    opq()%>%
    add_osm_feature(key = "junction",
                    value = "roundabout") %>%
    osmdata_sf(quiet = F,
               stringsAsFactors = F) 
  if(nrow(roundabouts$osm_polygons) > 0){
      roundabouts <-  roundabouts$osm_polygons[,c("osm_id",
                                               "junction")]
      
  roundabouts <- roundabouts %>% st_transform(target_projection)
    
  } else {
    nrows <- 1
    roundabouts <- st_sf(osm_id = 1:nrows, 
                "junction" = 1:nrows,
                geometry = st_sfc(lapply(1:nrows, function(x) st_geometrycollection())))
    roundabouts <- roundabouts[-1,]
  }
  
  
  return(roundabouts)
}

################################################################################
# check if highways belong to a bike route
################################################################################

in_bike_route <- function(highways, bike_routes){
  highways$local_cycle_network <- F
  highways$local_cycle_network_name <- NA
  highways$regional_cycle_network <- F
  highways$regional_cycle_network_name <- NA
  highways$national_cycle_network <- F
  highways$national_cycle_network_name <- NA
  
  if(length(bike_routes$osm_multilines) > 0){
    for(i in 1:nrow(highways)){
      print(i/nrow(highways))
      routes <- bike_routes %>%
        osm_multilines(highways$osm_id[i])
      if(nrow(routes) > 0){
        print(i)
        highways[i, ]$local_cycle_network <- ifelse("lcn" %in% routes$network,
                                                    T,
                                                    F)
        highways[i, ]$local_cycle_network_name <- ifelse("lcn" %in% routes$network,
                                                    paste0(routes[which(routes$network %in% "lcn"),]$name),
                                                    NA)
        highways[i, ]$regional_cycle_network <- ifelse("rcn" %in% routes$network,
                                                       T,
                                                       F)
        highways[i, ]$regional_cycle_network_name <- ifelse("rcn" %in% routes$network,
                                                         paste0(routes[which(routes$network %in% "rcn"),]$name),
                                                         NA)
        highways[i, ]$national_cycle_network <- ifelse("ncn" %in% routes$network,
                                                       T,
                                                       F)
        highways[i, ]$national_cycle_network_name <- ifelse("ncn" %in% routes$network,
                                                            paste0(routes[which(routes$network %in% "ncn"),]$name),
                                                            NA)
      }
    }
  }
  
  return(highways)
}

in_bike_route_postgres <- function(highways, bike_routes){
  
  get_bike_route_ids <- function(parts){
    str_extract_all(parts, "\\d+") %>% 
      unlist %>%
      unique() %>%
      as.integer()
  }
  
  get_bike_route_names <- function(osm_id, routes){
    routes_index <- str_detect(bike_routes$parts, as.character(osm_id))
    matching_routes <- bike_routes[routes_index, ]
    
    names <- paste(matching_routes$name, 
                   collapse = " | ")
    
    return(names)
  }
  
  lcn_ids <- get_bike_route_ids(bike_routes[which(bike_routes$lcn), ]$parts) %>% 
    unique()
  rcn_ids <- get_bike_route_ids(bike_routes[which(bike_routes$rcn), ]$parts) %>% 
    unique()
  ncn_ids <- get_bike_route_ids(bike_routes[which(bike_routes$ncn), ]$parts) %>% 
    unique()
  
  highways$local_cycle_network <- highways$osm_id %in% lcn_ids
  highways$local_cycle_network_name <- sapply(X = highways$osm_id, 
                                              FUN = get_bike_route_names,
                                              routes = bike_routes[which(bike_routes$lcn), ])
  
  highways$regional_cycle_network <- highways$osm_id %in% rcn_ids
  highways$regional_cycle_network_name <- sapply(X = highways$osm_id, 
                                                 FUN = get_bike_route_names,
                                                 routes = bike_routes[which(bike_routes$rcn), ])
  
  highways$national_cycle_network <- highways$osm_id %in% ncn_ids
  highways$national_cycle_network_name <- sapply(X = highways$osm_id, 
                                                 FUN = get_bike_route_names,
                                                 routes = bike_routes[which(bike_routes$ncn), ])
  return(highways)
}

################################################################################
# check landcover
################################################################################

non_urban <- c(1, 2, 5, 6, 8, 10, 11, 12, 13, 14, 15, 16, 18, 19) # source: https://open.canada.ca/data/en/dataset/4e615eae-b90c-420b-adee-2ca35896caf6

in_natural_area_landcover <- function(ways){
  # for efficiency, only do paths
  ways <- ways %>% important_tags(desired_osm_columns)
  paths <- path(ways) & (bikes_indicated(ways) | cycletrack(ways))
  
  ways$natural <- NA
  
  check_natural <- function(feature){
    feature <- feature %>% st_sfc()
    st_crs(feature) <- st_crs(3347)
    feature.proj <- as(feature %>% 
                         st_transform(3978), "Spatial")
    lc <- raster::extract(landcover, feature.proj, buffer = natural_buffer_dist)
    return(any(unlist(lc) %in% non_urban))
  }
  print(paste0("Number of features: ", length(which(paths))))

  ways[which(paths), ]$natural <- lapply(X = st_geometry(ways[which(paths), ]),
                                         FUN = check_natural)
  
  return(ways$natural)
}

################################################################################
# check for turn restrictions
################################################################################
check_turn_restrictions <- function(highways, turn_restrictions){
  
  # add area measure to natural areas
  natural_areas$area <- st_area(natural_areas) %>% as.numeric()
  
  # count natural areas within buffer
  highways$nat_count <- lengths(st_intersects(st_buffer(highways,
                                                        natural_buffer_dist), 
                                              natural_areas %>%
                                                filter(!natural %in% exclude_natural 
                                                       & area > natural_min_area) %>% 
                                                st_make_valid()))
  return(highways)
}

################################################################################
# Find nearest highway for reference points
################################################################################

nearest_highway <- function(ground_pts, highways){
  ground_data_nn_dist <- st_nn(ground_pts,
                               highways,
                               k = 1,
                               returnDist = T)[[2]] %>% 
    unlist()
  
  ground_data_nn_index <- st_nn(ground_pts,
                                highways,
                                k = 1,
                                returnDist = T)[[1]] %>% 
    unlist()
  
  ground_data_nn_osm_id <- highways[ground_data_nn_index,]$osm_id
  
  ground_pts$nn_osm_id <- ground_data_nn_osm_id
  ground_pts$nn_dist <- ground_data_nn_dist
  
  return(ground_pts)
}

################################################################################
# select desired columns
################################################################################

desired_osm_columns <- c("osm_id", "name", "bicycle", "bridge", 
                         "cycleway", "cycleway.left", "cycleway.right", "cycleway.both",
                         "foot", "footway", "hgv", "highway", "horse",
                         "junction", "lanes", "lanes.backward", "lanes.forward", "lcn",
                         "lcn_ref", "lit", "maxspeed", "motor_vehicle", "oneway",
                         "oneway.bicycle", "path",
                         "railway", "segregated", "sidewalk", "sidewalk.both.surface",
                         "surface", "traffic_calming", "width", 
                         "local_cycle_network", "local_cycle_network_name",
                         "regional_cycle_network", "regional_cycle_network_name",
                         "national_cycle_network", "national_cycle_network_name",
                         "mtb.scale.imba", "mtb.scale", "mtb", "sac_scale", 
                         "access", "natural", "nat_count")
desired_ground_columns <- c("sample_id", "CSDNAME", "CSDUID",
                            "Can_BICS_stratum",
                            "Can_BICS_stratum_class", "streetview_image",
                            "street_view_image_date", "interpreter",
                            "reviewed", "revised_value_from_review", "notes",
                            "Can_BICS_ground", "Can_BICS_ground_class", 
                            "nn_dist", "nn_osm_id")

fncols <- function(data, cname) {
  add <-cname[!cname %in% names(data)]
  
  if(length(add)!=0) data[add] <- NA
  data
} 
# source: https://stackoverflow.com/questions/45857787/adding-column-if-it-does-not-exist

important_tags <- function(dataframe, tags){
  # add any missing columns
  dataframe <- dataframe %>%
    fncols(tags) %>%
    dplyr::select(tags)
  return(dataframe)
}

################################################################################
# Accuracy assessment
################################################################################
# recode ground data
ground_data_recode_infratype <- function(Can_BICS){
  Can_BICS_recode <- recode(Can_BICS,
                            "None" = "None",
                            "Cycle Track (on roadway)" = "Cycle Track",
                            "Non-Conforming Major Road" = "None",
                            "Non-Conforming Trail" = "None",
                            "Non-Conforming Other" = "None")
}

Can_BICS_class_confusion_matrix_display <- 
  function(Can_BICS_class_confusion_matrix,
           lengths,
           weights){
  lengths <- prediction_infratype$`length (km)`
  names(lengths) <- prediction_infratype$Can_BICS_recode
  lengths <- lengths[names(confusion_matrix)]
  lengths[7] <- sum(lengths, na.rm = T)
  names(lengths)[7] <- "Total"
  
  weights <- prediction_infratype$proportion
  names(weights) <- prediction_infratype$Can_BICS_recode
  weights <- weights[names(confusion_matrix)]
  weights[7] <- sum(weights, na.rm = T)
  names(weights)[7] <- "Total"
  
  confusion_matrix_display <- cbind(confusion_matrix, Length = round(lengths,1), Weight = round(weights,2))
}

Can_BICS_presence_confusion_matrix <- function(accuracy_assessment){
  
  mat_tab <- accuracy_assessment %>% as.matrix("xtabs") %>%
    data.frame()
  
  names(mat_tab) <- presence_levels
  row.names(mat_tab) <- presence_levels
  
  
  mat_tab$Total <- rowSums(mat_tab[,])
  mat_tab[nrow(mat_tab) + 1,] <- colSums(mat_tab)
  rownames(mat_tab)[nrow(mat_tab)] <- "Total"
  
  return(mat_tab)
}

Can_BICS_class_accuracy <- function(accuracy_assessment){
  
  class_tab <- accuracy_assessment %>% as.matrix("classes") %>%
    data.frame()
  
  names(class_tab) <- infra_levels
  
  return(class_tab[c("Sensitivity",
                     "Specificity",
                     "Precision",
                     "Recall"),])
}

Can_BICS_overall_accuracy <- function(accuracy_assessment){
  
  overall_tab <- accuracy_assessment %>% as.matrix("overall") %>%
    data.frame() 
  
  stat_names <- row.names(overall_tab)
  
  overall_tab <- overall_tab %>%
    as_tibble()
  
  overall_tab$Statistic <- stat_names
  names(overall_tab) <- c("Value", "Statistic")
  
  overall_tab[ , c("Statistic", "Value")] # re-order
  
  return(overall_tab)
}

Can_BICS_accuracy_assessment <- function(reference,
                                         predicted,
                                         common_levels){
  # convert to factors with the same order
  reference_levels = factor(reference,
                            levels = common_levels)
  
  predicted_levels = factor(predicted, 
                            levels = common_levels)
  
  confusion_matrix <- table(predicted_levels,
                            reference_levels)
  
  confusion_matrix <- as.data.frame.matrix(confusion_matrix)
  
  return(confusion_matrix)
}

Can_BICS_confusion_matrix_display <- function(confusion_matrix,
                                              highways,
                                              categories){
  # add totals
  confusion_matrix$Total <- rowSums(confusion_matrix)
  confusion_matrix[nrow(confusion_matrix) + 1, ] <- colSums(confusion_matrix)
  row.names(confusion_matrix)[nrow(confusion_matrix)] <- "Total"

  # add lengths and weights
  lengths_weights <- get_lengths_proportions(highways,
                                             categories) %>%
    data.frame()
  
  lengths_weights[nrow(lengths_weights) + 1, ] <- NA
  lengths_weights$Category <- as.character(lengths_weights$Category)
  lengths_weights[nrow(lengths_weights), ]$Category <- "Total"
  lengths_weights[nrow(lengths_weights), ]$Length <- sum(lengths_weights$Length, 
                                                         na.rm = T)
  lengths_weights[nrow(lengths_weights), ]$Proportion <- sum(lengths_weights$Proportion, 
                                                             na.rm = T)
  
  row.names(lengths_weights) <- lengths_weights$Category
  
  # sort to match confusion matrix
  lengths_weights <- lengths_weights[row.names(confusion_matrix), ]
  
  confusion_matrix_display <- cbind(confusion_matrix, 
                                    lengths_weights)
  
  confusion_matrix_display <- confusion_matrix_display %>%
    select(-Category)
  
  return(confusion_matrix_display)
}

Can_BICS_accuracy_assessment_by_city <- function(reference,
                                                 predicted,
                                                 common_levels,
                                                 CSDNAME,
                                                 selected_CSDNAME){
  city <- which(CSDNAME %in% selected_CSDNAME)
  
  confusion_matrix <- Can_BICS_accuracy_assessment(
    reference[city],
    predicted[city],
    common_levels
  )
  
  return(confusion_matrix)
}


################################################################################
# accuracy assessment
area_error_matrix <- function(error_matrix){
  # this function expects an error matrix with counts of samples with a total
  # column returned from the function "Can_BICS_confusion_matrix"
  
  # the weights should be proportions (<1) and have the same names
  
  # remove the totals
  if("Total" %in% names(error_matrix)){ 
    error_matrix <- error_matrix[-which(row.names(error_matrix) %in% "Total"), ]
  }
  
  # make an empty matrix the same size
  area_error_matrix <- error_matrix
  area_error_matrix[] <- NA
  
  for(i in 1:nrow(error_matrix)){
    for(j in 1:ncol(error_matrix)){
      if(sum(error_matrix[i, 1:nrow(error_matrix)]) == 0){
        area_error_matrix[i,j] <- 0
      } else {
        area_error_matrix[i,j] <- error_matrix[i, ]$Proportion * 
          (error_matrix[i,j] / error_matrix[i, ]$Total)
      }
    }
  }
  
  # add totals
  area_error_matrix$Total <- rowSums(area_error_matrix[ ,1:nrow(area_error_matrix)])
  area_error_matrix[nrow(area_error_matrix) + 1,] <- colSums(area_error_matrix)
  rownames(area_error_matrix)[nrow(area_error_matrix)] <- "Total"
  
  area_error_matrix <- area_error_matrix %>%
    select(-c(Length, Proportion))
  
  return(area_error_matrix)
}

error_matrix_area_display <- function(
  error_matrix,
  error_matrix_area){
 # overall accuracy
  overall_accuracy_calc <- overall_accuracy(error_matrix_area)
  overall_accuracy_calc_ci <- overall_accuracy_ci(error_matrix,
                                                  error_matrix_area)
  
  # sensitivity, specificity
  sensitivity_display <- sensitivity_calc(error_matrix_area) %>% round(2) * 100
  
  specificity_display <- specificity_calc(error_matrix_area) %>% round(2) * 100

  overall_display <- paste0(round(overall_accuracy_calc, 2) * 100,
                            " ±",
                            round(overall_accuracy_calc_ci, 2) * 100)
  overall_display[2:nrow(error_matrix_area)] <- NA
  
  confusion_matrix_area_display <- cbind(round(error_matrix_area, 3)*100,
                                         `Sensitivity` = sensitivity_display,
                                         `Specificity`= specificity_display,
                                         `Overall` = overall_display)
  
  return(confusion_matrix_area_display)
}
  
overall_accuracy <- function(area_error_matrix){
  
  # start at 0
  map_area_correct <- 0
  
  # ignore the totals
  if("Total" %in% names(area_error_matrix)){ 
    q <- ncol(area_error_matrix) - 1
  } else {
    q <- ncol(area_error_matrix)
  }
  for(i in 1:q){
    j <- i
    map_area_correct <- map_area_correct + area_error_matrix[i,j]
  }
  return(map_area_correct)
}

fill_in_zeros <- function(weights, names){
  # add any missing columns
  weights <- weights %>%
    fncols(names)
  weights[is.na(weights)] <- 0
  
  return(weights)
}

users_accuracies <- function(area_error_matrix){
  
  # remove the totals
  if("Total" %in% names(area_error_matrix)){ # quick check if a column is named total
    area_error_matrix <- area_error_matrix[1:(nrow(area_error_matrix)-1), 
                                           1:(ncol(area_error_matrix)-1)]
  }
  
  # set the number of classes. check for totals.
  if("Total" %in% names(area_error_matrix)){ 
    q <- ncol(area_error_matrix) - 1
  } else {
    q <- ncol(area_error_matrix)
  }
  
  # empty vector with names 
  users_accuracies <- area_error_matrix[1,]
  users_accuracies[] <- NA
  
  for(i in 1:q){
    j <- i
    if(sum(area_error_matrix[i,]) > 0){
      users_accuracies[i] <- area_error_matrix[i,j] / sum(area_error_matrix[i,])
    } else {
      users_accuracies[i] <- NA
    }
  }
  return(users_accuracies)
}

producers_accuracies <- function(area_error_matrix){
  
  # remove the totals
  if("Total" %in% names(area_error_matrix)){ # quick check if a column is named total
    area_error_matrix <- area_error_matrix[1:(nrow(area_error_matrix)-1), 
                                           1:(ncol(area_error_matrix)-1)]
  }
  
  # set the number of classes. check for totals.
  if("Total" %in% names(area_error_matrix)){ 
    q <- ncol(area_error_matrix) - 1
  } else {
    q <- ncol(area_error_matrix)
  }
  
  # empty vector with names 
  producers_accuracies <- area_error_matrix[1,]
  producers_accuracies[] <- NA
  
  for(i in 1:q){
    j <- i
    if(sum(area_error_matrix[j,]) > 0){
      producers_accuracies[j] <- area_error_matrix[i, j] / sum(area_error_matrix[, j])
    } else {
      producers_accuracies[j] <- NA
    }
  }
  return(producers_accuracies)
}

overall_accuracy_ci <- function(confusion_matrix,
                                confusion_matrix_area){
  
  weights <- confusion_matrix_area$Total
  names(weights) <- names(confusion_matrix_area)
  weights <- weights[-which(names(weights) %in% "Total")]
  
  users_accuracies <- users_accuracies(confusion_matrix_area)
  
  nobs_map <- confusion_matrix %>% rowSums()
  names(nobs_map) <- names(confusion_matrix)

  # check the order matches
  if(! identical(names(users_accuracies), names(weights))){
    print("attempting to reorder weights.")
    weights <- weights[names(users_accuracies)]
  }
  if(! identical(names(users_accuracies), names(nobs_map))){
    print("attempting to reorder nobs_map")
    nobs_map <- nobs_map[names(users_accuracies)]
  }
  
  # ignore the totals
  if("Total" %in% names(weights)){ 
    q <- length(weights) - 1
  } else {
    q <- length(weights)
  }
  
  # empty vector with names 
  overall_accuracy_ci <- users_accuracies
  overall_accuracy_ci[] <- NA
  
  for(i in 1:q){
    j <- i
    if(weights[i] > 0){
      overall_accuracy_ci[i] <- (weights[i]^2 * users_accuracies[i] * 
                                   (1 - users_accuracies[i])) / (nobs_map[i] - 1)
    } else {
      overall_accuracy_ci[i] <- NA
    }
  }
  return(1.96 * sqrt(sum(overall_accuracy_ci, na.rm = T)))
}

negatives <- function(confusion_matrix){
  total <- "Total" %in% names(confusion_matrix)
  n_cats <- length(confusion_matrix)
  counts <- rep(NA, n_cats)
  
  # if there's a total, don't include it in the counts
  if(total){
    n_cats <- n_cats - 1
  }
  
  for(i in 1:n_cats){
    exclude_rows <- i
    if(total){
      exclude_rows <- c(exclude_rows, which(names(confusion_matrix) %in% "Total"))
      exclude_cols <- which(names(confusion_matrix) %in% "Total")
    }
    
    counts[i] <- sum(confusion_matrix[- exclude_rows, -exclude_cols])
  }
  return(counts)
}

true_positives <- function(confusion_matrix){
  total <- "Total" %in% names(confusion_matrix)
  n_cats <- length(confusion_matrix)
  counts <- rep(NA, n_cats)
  
  # if there's a total, don't include it in the counts
  if(total){
    n_cats <- n_cats - 1
  }
  
  for(i in 1:n_cats){
    counts[i] <- confusion_matrix[i, i]
  }
  return(counts)
}

false_positives <- function(confusion_matrix){
  total <- "Total" %in% names(confusion_matrix)
  n_cats <- length(confusion_matrix)
  counts <- rep(NA, n_cats)
  
  # if there's a total, don't include it in the counts
  if(total){
    n_cats <- n_cats - 1
  }
  
  for(i in 1:n_cats){
    exclude <- i
    if(total){
      exclude <- c(exclude, which(names(confusion_matrix) %in% "Total"))
    }
    
    counts[i] <- sum(confusion_matrix[i,- exclude])
  }
  return(counts)
}

true_negatives <- function(confusion_matrix){
  total <- "Total" %in% names(confusion_matrix)
  n_cats <- length(confusion_matrix)
  counts <- rep(NA, n_cats)
  
  # if there's a total, don't include it in the counts
  if(total){
    n_cats <- n_cats - 1
  }
  
  for(i in 1:n_cats){
    exclude <- i
    if(total){
      exclude <- c(exclude, which(names(confusion_matrix) %in% "Total"))
    }
    
    counts[i] <- sum(confusion_matrix[- exclude, - exclude])
  }
  return(counts)
}

false_negatives <- function(confusion_matrix){
  total <- "Total" %in% names(confusion_matrix)
  n_cats <- length(confusion_matrix)
  counts <- rep(NA, n_cats)
  
  # if there's a total, don't include it in the counts
  if(total){
    n_cats <- n_cats - 1
  }
  
  for(i in 1:n_cats){
    exclude <- i
    if(total){
      exclude <- c(exclude, which(names(confusion_matrix) %in% "Total"))
    }
    
    counts[i] <- sum(confusion_matrix[- exclude, i])
  }
  return(counts)
}

sensitivity_calc <- function(confusion_matrix){
  true_positives(confusion_matrix) /
    (true_positives(confusion_matrix) + false_negatives(confusion_matrix))
}

specificity_calc <- function(confusion_matrix){
  true_negatives(confusion_matrix) /
    (true_negatives(confusion_matrix) + false_positives(confusion_matrix))
}

################################################################################
# summary stats

get_lengths_proportions <- function(features,
                                    categories){
  
  # get the proportion of each class
  features$length <- st_length(features) %>% as.numeric() / 1000
  
  features$category <- categories
  
  prediction_infratype <- features %>%
    st_drop_geometry() %>%
    filter(! is.na(categories))
  
  prediction_infratype <- prediction_infratype %>%
    group_by(category) %>%
    summarise(
      length = sum(length) %>% 
        round(1) %>%
        as.numeric()) %>%
    mutate(proportion = round((length / sum(length)), 6) %>%
             as.numeric())
  
  names(prediction_infratype) <- c("Category",
                                   "Length",
                                   "Proportion")
  
  return(prediction_infratype)
}

get_lengths_proportions_groups <- function(features,
                                           categories,
                                           groups){
  
  # get the proportion of each class
  features$length <- st_length(features) %>% as.numeric() / 1000
  features$category <- categories
  features$group <- groups
  
  prediction_infratype <- features %>%
    st_drop_geometry() %>%
    filter(! is.na(categories))
  
  prediction_infratype <- prediction_infratype %>%
    group_by(group, category) %>%
    summarise(
      length = sum(length) %>% 
        round(1) %>%
        as.numeric()) %>%
    mutate(proportion = round((length / sum(length)), 6) %>%
             as.numeric())
  
  names(prediction_infratype) <- c(
    "Group",
    "Category",
    "Length",
    "Proportion")
  
  return(prediction_infratype)
}
