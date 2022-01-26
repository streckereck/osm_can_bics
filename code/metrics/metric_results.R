library(tidyr)
library(dplyr)
library(sf)

### LOAD DATA

load(file = "Results/Can-BICS Spatial Metric.rdata")


#Summary statistics by high/medium/low/total/weighted total
summary(metrics[,c("totalkm_wt_perarea","totalkm_perarea","high_perarea", "med_perarea","low_perarea")])

#Correlations by high/medium/low/total/weighted total
can_ale_r <- metrics %>%
  drop_na(ale_index) %>%
  summarise(r_total_wt = cor(ale_index, totalkm_wt_perarea), r_total = cor(ale_index, totalkm_perarea), r_high = cor(ale_index, high_perarea),
            r_med = cor (ale_index, med_perarea), r_low = cor(ale_index, low_perarea))

bike_r <- metrics %>%
  drop_na(bike_per) %>%
  summarise(r_total_wt = cor(bike_per, totalkm_wt_perarea), r_total = cor(bike_per, totalkm_perarea), r_high = cor(bike_per, high_perarea),
            r_med = cor (bike_per, med_perarea), r_low = cor(bike_per, low_perarea))

at_r <- metrics %>%
  drop_na(at_per) %>%
  summarise(r_total_wt = cor(at_per, totalkm_wt_perarea), r_total = cor(at_per, totalkm_perarea), r_high = cor(at_per, high_perarea),
            r_med = cor (at_per, med_perarea), r_low = cor(at_per, low_perarea))


### ANALYSIS BY CAN-BICS CATEGORY 

#average weighted km/km2 by category
metrics %>%
  group_by(category) %>%
  summarise(mean_km_wt = mean(totalkm_wt_perarea))

#Frequency of each category
category_count <- metrics %>%
  count(category) %>%
  mutate(per = n / sum(n))

#Percentage of Population (DA population) by category
category_pop <- metrics %>%
  group_by(category) %>%
  summarize(pop = sum(Population))%>%
  mutate(per = pop / sum(pop))


##INSIDE vs. OUTSIDE CMA COMPARISON

metrics$inCMA <- ifelse(metrics$CMATYPE == "B", "CMA", "Outside CMA")
metrics$inCMA[is.na(metrics$inCMA)] <- "Outside CMA"

cma_dist <- metrics %>%
  group_by(inCMA) %>%
  count(category) %>%
  group_by(inCMA) %>%
  mutate(per = n / sum(n))

cma_pop <- metrics %>%
  group_by(inCMA, category) %>%
  summarize(pop = sum(Population))%>%
  mutate(per = pop / sum(pop))

#Calculate average bike & active transport to work rates by category
category_bike <- metrics %>%
  drop_na(bike_per) %>%
  group_by(category) %>%
  summarize(bike = mean(bike_per)) 

category_at <- metrics %>%
  drop_na(at_per) %>%
  group_by(category) %>%
  summarize(at = mean(at_per))

#calculate average weighted sum (km/km2) by category
category_wt_km <- metrics %>%
  group_by(category) %>%
  summarize(weighted_sum = mean(totalkm_wt_perarea))

### CORRELATIONS by CITY

cities <- c("Toronto", "Edmonton", "Ottawa", "Montréal", "Vancouver", "Winnipeg", "Halifax", "Regina", "St. John's", 
            "Calgary", "Québec", "London", "Victoria", "Saskatoon", "Charlottetown", "Whitehorse")

correlations_city <- metrics %>%
  filter(CSDNAME %in% cities) %>%
  drop_na(ale_index) %>%
  group_by(CSDNAME) %>%
  summarise(r_bike = cor(bike_per, totalkm_wt_perarea), r_at = cor(at_per, totalkm_wt_perarea), r_can_ale = cor (ale_index, totalkm_wt_perarea), )

