library(tidyr)
library(dplyr)
library(sf)
library(ggrepel)
library(viridis)
options(scipen=10000)

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

metrics$inCMA <- ifelse(metrics$CMATYPE != "B" | is.na(metrics$CMATYPE), "Outside CMAs", "Within CMAs")

cma_dist <- metrics %>%
  group_by(inCMA) %>%
  count(category) %>%
  group_by(inCMA) %>%
  mutate(per = (round(n / sum(n),3)))

cma_pop <- metrics %>%
  group_by(inCMA, category) %>%
  summarize(pop = sum(Population))%>%
  mutate(per = pop / sum(pop))

summary(cma_dist$per)

#plot distribution of DAs by category within vs. outside CMAs - BUBBLE PLOT
colours = c( "#9C9C9C",  "#B3CDE3", "#8C96C6", "#8856A7","#810F7C")

ggplot(cma_dist, aes(x = inCMA, y = category, label = per)) + geom_point(aes(size = per, fill = factor(category)), shape = 21) + theme_minimal() +
  scale_size_continuous(range = c(2,24))+ 
  labs( x= "", y = "Can-BICS Category\n", size = "Number of DAs", fill = "")  + 
  theme(legend.key=element_blank(),
        axis.text.x = element_text(colour = "black", size = 12, face = "bold"),
        axis.text.y = element_text(colour = "black", size = 11),
        axis.title.y = element_text(face = "bold"),
        legend.text = element_text(size = 10, face ="bold", colour ="black"),
        legend.title = element_text(size = 12, face = "bold"),
        legend.position = "right")+ 
  scale_y_discrete(limits = c("(Lowest)  1", "2", "3", "4", "(Highest)  5"))+  
  scale_x_discrete(labels = c("% of DAs \n outside CMAs", "% of DAs \n within CMAS"))+  
  scale_fill_manual(values = colours) + 
  geom_label(data=cma_dist, aes(label=paste0(per*100,"%")), size=4, hjust = -0.8)+ theme(legend.position="none")


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

correlations_CSDs <- metrics %>%
  filter(CSDNAME %in% cities) %>%
  drop_na(ale_index) %>%
  group_by(CSDNAME) %>%
  summarise(r_bike = cor(bike_per, totalkm_wt_perarea), r_at = cor(at_per, totalkm_wt_perarea), r_can_ale = cor (ale_index, totalkm_wt_perarea), )

### PLOT CORRELATIONS - with csd populations and kms of infrastructure 

## read in CSD population data
CSD_pop <- read.csv("C:/Users/jenev/sfuvault2/Jeneva/Can-BICS/CSD Populations.csv", encoding = "UTF-8")
names(CSD_pop) <- c("CSDNAME", "Population")

#calculate km of infrastructure in each CSD

##Can-BICS classified Data 
lines <- st_read("C:/Users/jenev/Dropbox/Can-BICS_Existing cohorts/osm_infrastructure/data/provincial_data/canada_data/highways_output_v2_1.shp")

lines$CSDNAME <- gsub("QuÃ©bec","Québec", lines$CSDNAME)
lines$CSDNAME <- gsub("MontrÃ©al", "Montréal", lines$CSDNAME)

infra <- lines %>% 
  mutate(length = st_length(.) %>% as.numeric()) 

CSD_km <- infra %>%
  filter(CSDNAME %in% cities) %>%
  filter(C_BICS_ != "Non-Conforming") %>% #exclude non-conforming bike lanes 
  as_tibble() %>% 
  group_by(CSDNAME)%>%
  summarize(km = sum(length)/1000)


correlations_CSDs <- merge(correlations_CSDs, CSD_pop, by = "CSDNAME")
correlations_CSDs <- merge(correlations_CSDs, CSD_km, by = "CSDNAME")

#PLOT CSD correlations - scatterplot 
ggplot(correlations_CSDs, aes(x = Population, y = r_can_ale, label = CSDNAME)) + geom_point(aes(size = km,fill = r_can_ale), shape = 21) + theme_minimal() +
  geom_text_repel(hjust = -0.4)+
  scale_fill_viridis(option = "B", direction = -1, guide = "none")+ 
  labs( x= "Population", y = "Correlation with Can-ALE Index\n", size = "Total kms", fill = "")+ scale_x_continuous(labels = scales::comma)

ggplot(correlations_CSDs, aes(x = Population, y = r_bike, label = CSDNAME)) + geom_point(aes(size = km,fill = r_can_ale), shape = 21) + theme_minimal() +
  geom_text_repel(hjust = -0.5)+
  scale_fill_viridis(option = "B", direction = -1, guide = "none")+ 
  labs( x= "Population", y = "Correlation with bike-to-work rate\n", size = "Total kms", fill = "")+ scale_x_continuous(labels = scales::comma)


