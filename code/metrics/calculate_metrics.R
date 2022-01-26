# This code calculates Can-BICS metrics for all DAs in Canada

library(sf)
library(tidyverse)
library(raster)
library(cancensus)
library(cluster)

# library(Gmedian)
# library(ggplot2)
#library(RColorBrewer)
# library(factoextra)
# library(data.table)
# library(rgdal)
# library(corrplot)
options(scipen = 999)

#setwd("C:/Users/jenev/sfuvault/Jeneva/Can-BICS")

### LOAD SPATIAL DATA

#Dissemination area Boundary shapefile
DAs <- st_read("Data/DAs/lda_000b16a_e.shp")

##Can-BICS classified Data 
lines <- st_read("Data/OSM Can-BICS/highways_output_v2_1.shp")

#Population weighted DA centroids
wt_centroids <- read.csv("Data/DAs/2016_92-151_XBB.csv")
wt_centroids <- wt_centroids[!duplicated(wt_centroids[,c('DAuid.ADidu')]),] # select unique DA rows
wt_centroids <- st_as_sf(wt_centroids, coords = c("DArplong.ADlong", "DArplat.Adlat"), crs = 4326) %>% 
  st_transform(crs(lines)) # project to statscan lambert conformal conic
crs(lines)
### LOAD CENSUS & CAN-ALE DATA FOR CORRELATION ANALYSIS 

##Can-ALE Dataset
can_ale <- read.csv("Data/Can-ALE/CanALE_2016.csv")

#Cancensus Package set API
api <- source("Data/API_key.R")
options(cancensus.api_key = api$visible)
options(cancensus.cache_path = api$value)

#Census: extract census bike, walk, & transit to work variables for all DAs in Canada 
census_data <- get_census(dataset='CA16', regions=list(C="01"), 
                          vectors=c("v_CA16_5807", "v_CA16_5804","v_CA16_5801"), level='DA')

#Calculate active transportation to work total (sum of walk/bicycle/public transit)
census_data$at <- rowSums(census_data[,c("v_CA16_5807: Bicycle", "v_CA16_5804: Walked", "v_CA16_5801: Public transit")])


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
  filter(C_BICS_ != "Non-Conforming") %>% #exclude non-conforming bike lanes 
  as_tibble() %>% 
  group_by(DAuid.ADidu, C_BICS_) %>%
  summarize(total_km = sum(length)/1000) #convert from m to km

#create separate columns for can-BICS classes
length_cbics <- spread(length_cbics, C_BICS_, total_km)

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


### CALCULATE CENSUS VARIABLE RATES IN BUFFERS - BASED ON DA AREAS 
Buffer_DA_Areas <- merge(Buffer_DA_Areas, census_data, by.x = "DAUID", by.y="GeoUID", all=TRUE)

#calculate percent of bike to work/active transport to work/population in buffer
Buffer_DA_Areas$bike <- Buffer_DA_Areas$per_area*Buffer_DA_Areas$`v_CA16_5807: Bicycle`
Buffer_DA_Areas$at <- Buffer_DA_Areas$per_area*Buffer_DA_Areas$at
Buffer_DA_Areas$pop <- Buffer_DA_Areas$per_area*Buffer_DA_Areas$Population

#group by buffer and sum bike to work/active transport/population in buffer 
Buffer_rates <- Buffer_DA_Areas[,c("ID", "bike", "at", "pop", "area")] %>%
  group_by(ID) %>%
  summarize(bike_b = sum(bike, na.rm = TRUE), at_b = sum(at, na.rm = TRUE), pop_b = sum(pop, na.rm=TRUE), DA_area_sum = sum(area, na.rm = TRUE))

#Join rates with buffers dataframe / census data
buffers <- merge(buffers, Buffer_rates, by = "ID", all=TRUE)
census_data <- merge(census_data, buffers[c("DAuid.ADidu", "bike_b", "at_b", "pop_b", "DA_area_sum")], by.x = "GeoUID", by.y = "DAuid.ADidu", all = TRUE)

#calculate bike to work and active transport to work rates
census_data$bike_per <- census_data$bike_b / census_data$pop_b
census_data$at_per <- census_data$at_b / census_data$pop_b

#merge buffer rates, DA population, & Can-ALE data with Can-BICS data
df <- merge(census_data[c("GeoUID", "bike_per", "at_per", "DA_area_sum", "Population")], merged, by.x = "GeoUID", by.y = "DAUID", ALL = TRUE)
df <- merge(can_ale[c("dauid","ale_index", "ale_class")], df, by.x = "dauid", by.y = "GeoUID", ALL = TRUE)

#convert can-ale indices to numeric
df$ale_index <- as.numeric(as.character(df$ale_index))


#### CALCULATE KM / KM2 OF INFRASTRUCTURE FOR EACH DA
df$totalkm_perarea <- df$total_km/df$DA_area_sum
df$totalkm_wt_perarea <- df$total_km_w/df$DA_area_sum
df$high_perarea <- df$High_comfort/df$DA_area_sum
df$med_perarea <- df$Medium_comfort/df$DA_area_sum
df$low_perarea <- df$Low_comfort/df$DA_area_sum

#replace NAs with 0 (0km present)
df[,37:41][is.na(df[,37:41])] <- 0
summary(df)
 
### K-MEDIANS CLUSTERING OF TOTAL WEIGHTED KMS

total_km_wt <- as.data.frame(df$totalkm_wt_perarea)
total_km_wt <- as.matrix(total_km_wt)

#cluster into 5 categories: pamonce = 3 for faster k-medians clustering algorithm 
pam.res <- pam(total_km_wt, 5, pamonce = 3) 

#Range for each cluster
pam.res$medoids

#join clustering results back to original data 
metrics <- cbind(df, cluster = pam.res$cluster)

#re-label cluster in order - class = Can-BICS category from 1 to 5
metrics$category <- metrics$cluster
metrics$category[metrics$cluster==1] <- 2
metrics$category[metrics$cluster==2] <- 3
metrics$category[metrics$cluster==3] <- 1

#distribution by cluster
hist(metrics$category)

#export to shapefile
st_write(metrics, dsn = "Results/CAN_BICS_metric.shp")

### save rdata
save(metrics, file = "Results/Can-BICS Spatial Metric.rdata")
