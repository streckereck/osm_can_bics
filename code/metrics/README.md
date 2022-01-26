# Can-BICS Metrics 
The Can-BICS metrics dataset includes measures of the cycling environment for all dissemination areas (DAs) in Canada. The metrics are based on the Can-BICS network dataset - a dataset of cycling infrastructure derived from [OpenStreetMap (OSM)](https://www.openstreetmap.org/) and classified according to the 
[Canadian Bikeway Comfort and Safety Classification System (Can-BICS)](https://www.canada.ca/en/public-health/services/reports-publications/health-promotion-chronic-disease-prevention-canada-research-policy-practice/vol-40-no-9-2020/canbics-classification-system-naming-convention-cycling-infrastructure.html). 


The Can-BICS metrics dataset, developed using version 2 of OSM Can-BICS (January 2022), is 
available to download from [ArcGIS online](https://arcg.is/0eyGy9).

### Table of contents

* [Can-BICS Metrics](#can-bics-metrics)
* [Technology](#technology)
* [Data acquisition](#data-acquisition-steps)
* [Processing](#processing)

### Can-BICS Metrics
The spatial unit for the Can-BICS metrics is a 1 km circular buffer around the population-weighted DA centroid. There are two summary metrics for each buffer area that are assigned to each dissemination area:


1. The **Can-BICS continuous metric**: A weighted sum of high-comfort kms (×3), medium-comfort kms (×2), and low-comfort kms (×1) within the buffer, normalized by the land area (km^2^) within each buffer. 

2. The **Can-BICS categorical metric**: 5 categories assigned using K-medians clustering of the continuous measure. Category 1 represents the lowest level of infrastructure, and Category 5 represents the highest level of infrastructure. 


<p align="center">
   <img src="../../figures/metric_buffer_2.png" width="800"/>
</p>
<p align="center">
      // **Figure 1** Can-BICS Metric calculation.

</p>

### Technology

* [R](https://www.r-project.org/) and [R Studio](https://www.rstudio.com/)
* R Packages:
  * [tidyverse](https://www.tidyverse.org/packages/)
  * [dplyr](https://www.rdocumentation.org/packages/dplyr/versions/0.7.8)
  * [sf](https://r-spatial.github.io/sf/)
  * [raster](https://cran.r-project.org/web/packages/raster/index.html)
  * [cancensus](https://mountainmath.github.io/cancensus/index.html) for correlation/validation analysis
  * [cluster](https://cran.r-project.org/web/packages/cluster/cluster.pdf) for the Can-BICS categorical metric

### Data acquisition steps

1. Download the OSM Can-BICS network dataset from [ArcGIS online](https://arcg.is/0eyGy9) and extract to `Data/OSM Can-BICS/`
2. Download the [Statistics Canada Census Dissemination Area Boundaries](https://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/bound-limit-2016-eng.cfm) in .shp format, 
and extract to `Data/DAs/`
3. Download the [2016 Statistics Canada Geographic Attribute File](https://www12.statcan.gc.ca/census-recensement/2011/geo/ref/att-eng.cfm) in .csv format.

To add the Can-ALE index and/or census DA-level data to the metric dataset (used for correlation/validation analysis):

4. Acquire and save the [Can-ALE Data](https://nancyrossresearchgroup.ca/research/can-ale/) in `Data/Can-ALE/`
5. Obtain a CensusMapper [API key](https://censusmapper.ca/users/sign_up). The [cancensus](https://mountainmath.github.io/cancensus/index.html) package retrieves census DA-level data in R and requires an API key to run. Update the API key and path in `/code/metrics/calculate_metrics.R`.



### Processing

1. `/code/metrics/calculate_metrics.R` calculates Can-BICS metrics for all DAs in Canada, and exports the data in a shapefile format. This file also calculates census bike-to-work rates and active-transport-to-work rates using a comparable method as the Can-BICS metrics (using a 1-km circular buffer around the population weighted DA centroid). All DAs intersecting the circular buffer are included and DA totals are weighted according to their proportional area within the buffer to calculate the buffer rate. 

  * If needed, adjust the inputs according to specific project goals. For example:
  
    - Subset the spatial data inputs to create a metrics shapefile for a specific geographic area. 
    - Adjust the weightings of the different infrastructure/comfort classes.
    - Create a different categorical ranking (i.e. with more or fewer categories).

2. `/code/metrics/metric_results.R` generates summary statistics, and outputs correlations with the Can-ALE index, census bike-to-work, and active-transport-to-work rates. This file requires the .rdata output from `/code/metrics/calculate_metrics.R`.
