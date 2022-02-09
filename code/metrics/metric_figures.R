library(sf)
library(ggrepel)
library(viridis)
library(corrplot)
library(RColorBrewer)
library(scales)
library(ggplot2)
library(dplyr)
options(scipen=10000)

### LOAD METRIC DATASET
load(file = "Results/Can-BICS Spatial Metric Jan 2022.rdata")

#Graph distribution of DAs by category
ggplot(metrics, aes(x = CBICS_cat)) +
  geom_bar(aes(fill = as.factor(CBICS_cat), y = (..count..)/sum(..count..))) + theme_minimal() +
  scale_fill_manual(values = c( "#9C9C9C",  "#B3CDE3", "#8C96C6", "#8856A7","#810F7C"))+ theme(legend.position="none") + 
  xlab("\nCan-BICS Category") +
  ylab("Percentage of DAs") + ggtitle("Distribution by Can-BICS Category\n") +
  theme(plot.title = element_text(hjust = 0.5))+
  geom_text(aes(label = scales::percent(accuracy = 1L, ..prop..),
                y= ..prop.. ), stat= "count", vjust = -.5) +theme(axis.text=element_text(size=12, face = "bold"))+
  theme(axis.title=element_text(size=14))+theme(plot.title=element_text(size=14))+
  theme(axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank())  #remove y axis ticks

## Histogram - Can-BICS continuous Metric
x1 <- expression("Can-BICS Continuous Metric" ~ (Weighted ~ total ~ kms ~ per ~ km^2))
ggplot(metrics, aes(x=CBICS_cont)) + 
  geom_histogram(color="black", fill="darkgrey", binwidth = 1,aes(y=..count../sum(..count..))) + theme_minimal() + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) +
  xlab(x1) +
  ylab("Percentage of DAs\n") + ggtitle("Distribution by Can-BICS Continuous Metric\n")+
  theme(plot.title = element_text(hjust = 0.5))+theme(axis.text=element_text(size=12, face = "bold"))+
  theme(axis.title=element_text(size=14))+theme(plot.title=element_text(size=14))


#Distribution of DAs by category within vs. outside CMAs - BUBBLE PLOT

##Distribution of DAs by category in vs. outside CMAs
metrics$inCMA <- ifelse(metrics$CMATYPE != "B" | is.na(metrics$CMATYPE), "Outside CMAs", "Within CMAs")
metrics$inCMA <- factor(metrics$inCMA, levels = c("Within CMAs", "Outside CMAs"))
cma_dist <- metrics %>%
  group_by(inCMA) %>%
  count(CBICS_cat) %>%
  group_by(inCMA) %>%
  mutate(per = (n / sum(n))*100)

specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))
colours <- c( "#9C9C9C",  "#B3CDE3", "#8C96C6", "#8856A7","#810F7C")

ggplot(cma_dist, aes(x = inCMA, y = CBICS_cat, label = per)) + geom_point(aes(size = per, fill = factor(CBICS_cat)), shape = 21) + theme_minimal() +
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
  scale_x_discrete(labels = c("% of DAs \n within CMAs", "% of DAs \n outside CMAS"))+  
  scale_fill_manual(values = colours) + 
  geom_label(data=cma_dist, aes(label=paste0(specify_decimal(per,1),"%")), size=4, hjust = -0.8)+ theme(legend.position="none")

#Correlation PLOT - correlation by high/medium/low/total km per km2 & continuous metric 
ale_r <- metrics %>%
  drop_na(ale_index) %>%
  summarise(r_total_wt = cor(ale_index, CBICS_cont), r_total = cor(ale_index, tot_km_km2), r_high = cor(ale_index, high_km2),
            r_med = cor (ale_index, med_km2), r_low = cor(ale_index, low_km2))

bike_r <- metrics %>%
  drop_na(bike_per) %>%
  filter(bike_per != 0) %>%
  summarise(r_total_wt = cor(bike_per, CBICS_cont), r_total = cor(bike_per, tot_km_km2), r_high = cor(bike_per, high_km2),
            r_med = cor (bike_per, med_km2), r_low = cor(bike_per, low_km2))

at_r <- metrics %>%
  drop_na(at_per) %>%
  filter_at(vars("at_per"), all_vars(!is.infinite(.))) %>% #remove 1 DA with main mode of commuting sample = 0 
  summarise(r_total_wt = cor(at_per, CBICS_cont), r_total = cor(at_per, tot_km_km2), r_high = cor(at_per, high_km2),
            r_med = cor (at_per, med_km2), r_low = cor(at_per, low_km2))

cor_matrix <- rbind(bike_r, at_r, ale_r)

rownames(cor_matrix) <- c("Bike-to-work rates","Active-transport-to-work rates","Can-ALE Index")
names(cor_matrix) <- c("Can-BICS continuous metric", "Total comfort kms/km2", "High comfort kms/km2", "Medium comfort kms/km2", 
                       "Low comfort kms/km2")
M <- data.matrix(cor_matrix , rownames.force = NA)

corrplot(M, method = "color", 
         addCoef.col = "black",
         tl.col="white",
         number.cex=1.2)
text(1.5:5.5, 4.35, cex = 1.2, font =2, srt = 40, expression("Can-BICS continuous metric", "Total comfort kms / km "^2, "High comfort kms / km "^2, "Medium comfort kms / km "^2, 
                                                             "Low comfort kms / km "^2))
text(-0.7, 3:1, cex = 1.2, font = 2, expression("Bike-to-work rates","Active-transport-to-work rates","Can-ALE Index"))


### PLOT CORRELATIONS by CSD - Scatterplot with CSD populations and kms of infrastructure within CSD
correlations_CSD <- read.csv("Results/CSD Correlations.csv")

#calculate km of infrastructure in each CSD
lines <- st_read("Data/OSM Can-BICS/OSM_CAN_BICS_latest.shp")
lines$CSDNAME <- gsub("QuÃ©bec","Québec", lines$CSDNAME)
lines$CSDNAME <- gsub("MontrÃ©al", "Montréal", lines$CSDNAME)

infra <- lines %>%
  mutate(length = st_length(.) %>% as.numeric())

CSD_km <- infra %>%
  filter(CSDNAME %in% cities) %>%
  filter(CBICS_comf != "Non-Conforming") %>% #exclude non-conforming bike lanes
  as_tibble() %>%
  group_by(CSDNAME)%>%
  summarize(km = sum(length)/1000)

correlations_CSDs <- merge(correlations_CSDs, CSD_km, by = "CSDNAME")

ggplot(correlations_CSDs, aes(x = Population, y = r_can_ale, label = CSDNAME)) + geom_point(aes(size = km), shape = 21, fill = "darkgreen") +
  theme_minimal() + geom_text_repel(hjust = -0.4)+
  labs( x= "\nPopulation", y = "Correlation with Can-ALE Index\n", size = "Total kms", fill = "")+
  scale_x_continuous(labels = scales::comma)+theme(axis.text=element_text(size=12, face = "bold"))+
  theme(axis.title=element_text(size=14))+theme(plot.title=element_text(size=14))+
  theme(legend.text=element_text(size=12), legend.title = element_text(size = 16))

ggplot(correlations_CSDs, aes(x = Population, y = r_bike, label = CSDNAME)) + geom_point(aes(size = km), shape = 21, fill = "darkgreen") +
  theme_minimal() + geom_text_repel(hjust = -0.5)+
  scale_fill_viridis(option = "B", direction = -1, guide = "none")+
  labs( x= "\nPopulation", y = "Correlation with bike-to-work rate\n", size = "Total kms", fill = "")+
  scale_x_continuous(labels = scales::comma) +
  theme(axis.text=element_text(size=12, face = "bold"))+
  theme(axis.title=element_text(size=14))+theme(plot.title=element_text(size=14))+
  theme(legend.text=element_text(size=12), legend.title = element_text(size = 16))


