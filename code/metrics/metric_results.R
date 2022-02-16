library(tidyr)
library(dplyr)
library(sf)

### LOAD METRIC DATASET
load(file = "Results/Can-BICS Spatial Metric Jan 2022.rdata")

#Summary statistics by high/medium/low/total/weighted total
summary(metrics[,c("CBICS_cont","tot_km_km2","high_km2", "med_km2","low_km2", "total_km")])

#number of DAs with no infrastructure
length(metrics$total_km[metrics$total_km == 0])
19213/56589 

#number of DAs with no high or medium infrastructure
nrow(metrics[metrics$high_comf==0 & metrics$med_comf==0, ])
22379/56589

##INSIDE vs. OUTSIDE CMA COMPARISON
metrics$inCMA <- ifelse(metrics$CMATYPE != "B" | is.na(metrics$CMATYPE), "Outside CMAs", "Within CMAs")
nrow(metrics[metrics$inCMA == "Within CMAs",])

#number of DAs in vs. outside CMAs
metrics %>% count(inCMA)
#population in vs. outside CMAs
metrics %>%
  group_by(inCMA) %>%
  summarize(pop = sum(Population)) %>%
  mutate(per = pop/sum(pop))


#number of DAs with no infrastructure in CMAs
nrow(metrics[metrics$total_km == 0 & metrics$inCMA == "Within CMAs",])
4719/36169

#number of DAs with no medium or high comfort infrastructure in CMAs
nrow(metrics[metrics$high_comf==0 & metrics$med_comf==0 & metrics$inCMA == "Within CMAs", ])
7026/36169

#Mean bike active transport rates in vs outside CMAs
metrics %>%
  group_by(inCMA)%>%
  drop_na(st_per) %>%
  filter_at(vars("st_per"), all_vars(!is.infinite(.))) %>%
  summarize(mean_at = mean(st_per))

metrics %>%
  group_by(inCMA)%>%
  drop_na(bike_per) %>%
  summarize(mean_bike = mean(bike_per))


### ANALYSIS BY CAN-BICS CATEGORY 

#summary statistics by category
tapply(metrics$CBICS_cont, metrics$CBICS_cat, summary)
tapply(metrics$tot_km_km2, metrics$CBICS_cat, summary)

#distribution of DAs by category
cat_dist <- metrics %>%
  count(CBICS_cat) %>%
  mutate(per = n / sum(n))

#Percentage of Population (DA population) by category
metrics %>%
  group_by(CBICS_cat) %>%
  summarize(pop = sum(Population))%>%
  mutate(per = pop / sum(pop))

#percentage of population by category in vs. outside CMAs
metrics %>%
  group_by(inCMA, CBICS_cat) %>%
  summarize(pop = sum(Population))%>%
  mutate(per = (pop / sum(pop))*100)



#DISTRIBUTION of DAs by category in 16 CSDs
cities <- c("Toronto", "Edmonton", "Ottawa", "Montréal", "Vancouver", "Winnipeg", "Halifax", "Regina", "St. John's", 
            "Calgary", "Québec", "London", "Victoria", "Saskatoon", "Charlottetown", "Whitehorse")

category_count_CSD <- metrics %>%
  filter(CSDNAME %in% cities) %>%
  group_by(CSDNAME) %>%
  count(CBICS_cat) %>%
  mutate(per = n / sum(n))
category_per_CSD <- spread(category_count_CSD[,c(1,2,4)], CBICS_cat, per)

## read in CSD population data
CSD_pop <- read.csv("Data/CSD Populations.csv", encoding = "UTF-8")
names(CSD_pop) <- c("CSDNAME", "Population")
## ADD CSD populations
category_per_CSD <- merge(category_per_CSD, CSD_pop, by = "CSDNAME")
category_per_CSD <- category_per_CSD[order(-category_per_CSD$Population),]

#export table
write.csv(category_per_CSD, "Results/CSD Category Distributions.csv", row.names = FALSE)

##Distribution of DAs by category in vs. outside CMAs
cma_dist <- metrics %>%
  group_by(inCMA) %>%
  count(CBICS_cat) %>%
  group_by(inCMA) %>%
  mutate(per = (n / sum(n))*100)

#AVERAGE bike & active transport to work rates by category

#bike to work rate within CMAs 
metrics %>%
  drop_na(bike_per) %>%
  group_by(CBICS_cat) %>%
  summarize(bike = round(mean(bike_per),3)) 

metrics %>%
  drop_na(st_per) %>%
  filter_at(vars("st_per"), all_vars(!is.infinite(.))) %>% #remove 1 DA with main mode of commuting sample = 0 
  group_by(CBICS_cat) %>%
  summarize(at = round(mean(st_per),3))


#Correlations by high/medium/low/total km per km2 & continuous metric 

metrics %>%
  drop_na(ale_index) %>%
  summarise(r_total_wt = cor(ale_index, CBICS_cont), r_total = cor(ale_index, tot_km_km2), r_high = cor(ale_index, high_km2),
            r_med = cor (ale_index, med_km2), r_low = cor(ale_index, low_km2))

metrics %>%
  drop_na(bike_per) %>%
  filter(bike_per != 0) %>%
  summarise(r_total_wt = cor(bike_per, CBICS_cont), r_total = cor(bike_per, tot_km_km2), r_high = cor(bike_per, high_km2),
            r_med = cor (bike_per, med_km2), r_low = cor(bike_per, low_km2))

metrics %>%
  drop_na(st_per) %>%
  filter_at(vars("st_per"), all_vars(!is.infinite(.))) %>% #remove 1 DA with main mode of commuting sample = 0 
  summarise(r_total_wt = cor(st_per, CBICS_cont), r_total = cor(st_per, tot_km_km2), r_high = cor(st_per, high_km2),
            r_med = cor (st_per, med_km2), r_low = cor(st_per, low_km2))

### CORRELATIONS BY CSD

#bike-to-work
correlations_CSDs_bike <- metrics %>%
  filter(CSDNAME %in% cities) %>%
  drop_na(bike_per) %>%
  group_by(CSDNAME) %>%
  summarise(r_bike = cor(bike_per, CBICS_cont))

#active-transport to work
correlations_CSDs_at <- metrics %>%
  filter(CSDNAME %in% cities) %>%
  drop_na(st_per) %>%
  filter_at(vars("st_per"), all_vars(!is.infinite(.))) %>%
  group_by(CSDNAME) %>%
  summarise(r_at = cor(st_per, CBICS_cont))

#can-ale
correlations_CSDs_ale <- metrics %>%
  filter(CSDNAME %in% cities) %>%
  drop_na(ale_index) %>%
  group_by(CSDNAME) %>%
  summarise(r_can_ale = cor (ale_index, CBICS_cont))

correlations_CSDs <- merge(correlations_CSDs_bike, correlations_CSDs_at, by = "CSDNAME")
correlations_CSDs <- merge(correlations_CSDs, correlations_CSDs_ale, by = "CSDNAME")
correlations_CSDs <- merge(correlations_CSDs, CSD_pop, by = "CSDNAME")

#save correlation table
correlations_CSDs <- correlations_CSDs[order(-correlations_CSDs$Population),]
write.csv(correlations_CSDs, "Results/CSD Correlations.csv", row.names = FALSE)

