# This code calculates Can-BICS metrics for all DAs in Canada

library(sf)
library(tidyverse)
library(raster)
library(cancensus)
library(cluster)
library(dplyr)

options(scipen = 999)

### LOAD SPATIAL DATA

#Dissemination area Boundary shapefile
DAs <- st_read("Data/DAs/lda_000b16a_e.shp")

##Can-BICS classified Data 
lines <- st_read("Data/OSM Can-BICS/OSM_CAN_BICS_latest.shp")

#Population weighted DA centroids
wt_centroids <- read.csv("Data/DAs/2016_92-151_XBB.csv")
wt_centroids <- wt_centroids[!duplicated(wt_centroids[,c('DAuid.ADidu')]),] # select unique DA rows
wt_centroids <- st_as_sf(wt_centroids, coords = c("DArplong.ADlong", "DArplat.Adlat"), crs = 4326) %>% 
  st_transform(crs(lines)) # project to statscan lambert conformal conic
### remove DA 35510090 - falls completely in coastal water and is not included in the DA boundary file
wt_centroids <- wt_centroids %>% filter(DAuid.ADidu != 35510090) 


### LOAD CENSUS & CAN-ALE DATA FOR CORRELATION ANALYSIS 

##Can-ALE Dataset
can_ale <- read.csv("Data/Can-ALE/CanALE_2016.csv")

#Cancensus Package set API
api <- source("Data/API_key.R")
options(cancensus.api_key = api$visible)
options(cancensus.cache_path = api$value)

#Census: extract census bike, walk, & transit to work variables for all DAs in Canada 
census_data <- get_census(dataset='CA16', regions=list(C="01"), 
                          vectors=c("v_CA16_5807", "v_CA16_5804","v_CA16_5801", "v_CA16_5792"), level='DA')

#Calculate sustainable transportation to work total (sum of walk/bicycle/public transit)
census_data$st <- rowSums(census_data[,c("v_CA16_5807: Bicycle", "v_CA16_5804: Walked", "v_CA16_5801: Public transit")])

#calculate DA bike-to-work and active transport-to-work rates
census_data$bike_per <- census_data$`v_CA16_5807: Bicycle`/census_data$`v_CA16_5792: Total - Main mode of commuting for the employed labour force aged 15 years and over in private households with a usual place of work or no fixed workplace address - 25% sample data`
census_data$st_per <- census_data$st/census_data$`v_CA16_5792: Total - Main mode of commuting for the employed labour force aged 15 years and over in private households with a usual place of work or no fixed workplace address - 25% sample data`

### CREATE BUFFERS AROUND CENTROIDS

#1km buffer
buffers <- st_buffer(wt_centroids, 1000, dissolve=FALSE)

### INTERSECT BUFFERS & CAN-BICS DATA

intersect <- st_intersection(lines, buffers)

### CALCULATE LENGTH OF BIKE LANES WITHIN BUFFERS

intersect_km <- intersect %>% 
  mutate(length = st_length(.) %>% as.numeric()) 


### CALCULATE KM OF BIKE LANES IN EACH DA BY CAN-BICS CLASS 

length_cbics <- intersect_km %>%
  filter(CBICS_comf != "Non-Conforming") %>% #exclude non-conforming bike lanes 
  as_tibble() %>% 
  group_by(DAuid.ADidu, CBICS_comf) %>%
  summarize(total_km = sum(length)/1000) #convert from m to km

#create separate columns for can-BICS classes
length_cbics <- spread(length_cbics, CBICS_comf, total_km)

#rename columns
names(length_cbics) <- c("DAUID", "High_comfort", "Medium_comfort", "Low_comfort")

#merge rows with same DAUID
length_cbics <- length_cbics %>% group_by(DAUID) %>% 
  summarise_all(funs(paste(., collapse = "-")))

#convert kms back to numeric
length_cbics[2:4] <- lapply(length_cbics[2:4],as.numeric)

#create total km column
length_cbics$total_km <- rowSums(length_cbics[,c("High_comfort", "Medium_comfort", "Low_comfort")], na.rm = TRUE)

#replace NAs with 0 (0km present)
length_cbics[is.na(length_cbics)] <- 0


### CALCULATE WEIGHTED SUM OF TOTAL KM

length_cbics$total_km_w <- length_cbics$High_comfort*3 + length_cbics$Medium_comfort*2 +length_cbics$Low_comfort*1
summary(length_cbics)

#join data with DA dataframe
merged <- merge(DAs, length_cbics, by = "DAUID", all = T)


### INTERSECT BUFFERS & CAN-BICS DATA / CALCULATE LAND AREA WITHIN BUFFERS 

#assign ID to each buffer for join
buffers <- tibble::rownames_to_column(buffers, "ID")

#intersect
DA_intersect <- st_intersection(st_make_valid(DAs), buffers[,c("ID", "geometry")])

#calculate area of each DA within buffers
DA_intersect <- DA_intersect %>% 
  mutate(area = st_area(.) %>% as.numeric())

Buffer_DA_Areas <- DA_intersect %>% 
  as_tibble() %>% 
  group_by(ID, DAUID) %>% 
  summarize(area = sum(area)/1000000)

#calculate percentage of DA area within buffer (1km buffer = 3.14km2)
Buffer_DA_Areas$per_area <- Buffer_DA_Areas$area / 3.140157

#group by buffer and sum total land area in buffer
buffer_land_areas <- Buffer_DA_Areas %>%
  group_by(ID) %>%
  summarize(buffer_land_area = sum(area, na.rm = TRUE))

#Join rates with buffers dataframe / census data
buffers <- merge(buffers, buffer_land_areas, by = "ID", all=TRUE)

census_data <- merge(census_data, buffers[c("DAuid.ADidu", "buffer_land_area")], by.x = "GeoUID", by.y = "DAuid.ADidu", all = TRUE)


#merge buffer rates, DA population, & Can-ALE data with Can-BICS data
df <- merge(census_data[c("GeoUID", "bike_per", "at_per", "buffer_land_area", "Population")], merged, by.x = "GeoUID", by.y = "DAUID", ALL = TRUE)
df <- merge(can_ale[c("dauid","ale_index", "ale_class")], df, by.x = "dauid", by.y = "GeoUID", ALL = TRUE)

#convert can-ale indices to numeric
df$ale_index <- as.numeric(as.character(df$ale_index))


#### CALCULATE KM / KM2 OF INFRASTRUCTURE FOR EACH DA
df$tot_km_km2 <- df$total_km/df$buffer_land_area
df$tot_wt_km2 <- df$total_km_w/df$buffer_land_area
df$high_km2 <- df$High_comfort/df$buffer_land_area
df$med_km2 <- df$Medium_comfort/df$buffer_land_area
df$low_km2 <- df$Low_comfort/df$buffer_land_area

#replace NAs with 0 (0km present)
df[,31:35][is.na(df[,31:35])] <- 0
df[,37:41][is.na(df[,37:41])] <- 0
summary(df)
 
### K-MEDIANS CLUSTERING OF TOTAL WEIGHTED KMS

total_km_wt <- as.data.frame(df$tot_wt_km2)
total_km_wt <- as.matrix(total_km_wt)

#cluster into 5 categories: pamonce = 3 for faster k-medians clustering algorithm 
pam.res <- pam(total_km_wt, 5, pamonce = 3) 

#Range for each cluster
pam.res$medoids

#join clustering results back to original data 
metrics <- cbind(df, cluster = pam.res$cluster)

#re-label cluster in order -> Can-BICS category from 1 to 5
metrics$CBICS_cat <- metrics$cluster
metrics$CBICS_cat[metrics$cluster==1] <- 2
metrics$CBICS_cat[metrics$cluster==2] <- 3
metrics$CBICS_cat[metrics$cluster==3] <- 1

#distribution by cluster
hist(metrics$CBICS_cat)

#select/rename columns for export (10 characters or less)
drop <- c("buffer_land_area","CDTYPE", "CCSUID", "CCSNAME", "CSDTYPE", "ERUID", "ERNAME", "SACCODE", "SACTYPE", 
          "CMAPUID", "CTUID", "CTNAME", "ADAUID", "X_Centroid", "Y_Centroid", "cluster")

metrics <- metrics[ , !(names(metrics) %in% drop)]
names(metrics)[names(metrics) == 'High_comfort'] <- 'high_comf'
names(metrics)[names(metrics) == 'Medium_comfort'] <- 'med_comf'
names(metrics)[names(metrics) == 'Low_comfort'] <- 'low_comf'
names(metrics)[names(metrics) == 'tot_wt_km2'] <- 'CBICS_cont'
metrics <- metrics %>% relocate(CBICS_cont, .before = last_col()) 

#export to shapefile
st_write(metrics, dsn = "Results/CAN_BICS_metric_Jan_2022.shp")

### save rdata
save(metrics, file = "Results/Can-BICS Spatial Metric Jan 2022.rdata")
