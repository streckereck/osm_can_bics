# OSM Can-BICS
OSM Can-BICS classifies [OpenStreetMap (OSM)](https://www.openstreetmap.org/) to 
create a network dataset of bicycle facilities, classified according to the 
[Canadian Bikeway Comfort and Safety Classification System (Can-BICS)](https://www.canada.ca/en/public-health/services/reports-publications/health-promotion-chronic-disease-prevention-canada-research-policy-practice/vol-40-no-9-2020/canbics-classification-system-naming-convention-cycling-infrastructure.html). The network dataset is used to build 
spatial metrics for all 
[dissemination areas](https://www12.statcan.gc.ca/census-recensement/2011/ref/dict/geo021-eng.cfm) 
in Canada.

![Bicycle infrastructure across Canada](figures/glow.png?raw=true "Bicycle facilities across Canada")

## Table of contents

* [Network dataset](#network-dataset)
* [Metric dataset](#metric-dataset)
* [Technology](#technology)


## Network dataset
**The national network dataset is available on 
[ArcGIS online](https://arcg.is/0PyqOu).**

To create bicycling facility datasets, many studies assemble open data provided 
by cities. However, compiling open data sources leads to inconsistent labeling 
of facilities across cities, and many studies do not provide a statement of 
accuracy. OpenStreetMap provides a more collaborative approach through a central 
repository and benefits from community contributors with on-the-ground 
knowledge. OSM Can-BICS provides a research-ready dataset complete with measures 
of bias and accuracy in 15 test cities. For more details, please see our 
[story map](https://arcg.is/1m4DXb).

More details and code are available here:

* Create the [national network dataset](./code/national).
* Perform an accuracy assessment in 15 [test cities](./code/test_cities).

## Metric dataset
**The national metric dataset is available on 
[ArcGIS online](https://arcg.is/0eyGy9).**

The Can-BICS metrics dataset includes measures of the cycling environment for 
all dissemination areas (DAs) in Canada. The spatial unit for the Can-BICS 
metrics is a 1 km circular buffer around the population-weighted DA centroid. 
The Can-BICS summary metrics include both a continuous measure (sum of km of cycling infrastructure per km<sup>2</sup>, weighted by Can-BICS comfort class), and a categorical measure.

More details and code are available here:

* Build [spatial metrics](./code/metrics) for all dissemination areas in 
Canada.

## Technology

* [R](https://www.r-project.org/) and [R Studio](https://www.rstudio.com/)
* R Packages:
  * [cancensus](https://mountainmath.github.io/cancensus/index.html)
  * [cluster](https://cran.r-project.org/web/packages/cluster/cluster.pdf) for 
the Can-BICS categorical metric
  * [ggridges](https://www.rdocumentation.org/packages/ggridges/versions/0.5.3)
  * [janitor](https://www.rdocumentation.org/packages/janitor/versions/2.1.0)
  * [lwgeom](https://cran.r-project.org/web/packages/lwgeom/index.html)
  * [mapview](https://r-spatial.github.io/mapview/)
  * [osmdata](https://github.com/ropensci/osmdata) for the test cities
  * [raster](https://cran.r-project.org/web/packages/raster/index.html)
  * [RPostgreSQL](https://cran.r-project.org/web/packages/RPostgreSQL/index.html) 
for the national dataset
  * [sf](https://r-spatial.github.io/sf/)
  * [testthat](https://testthat.r-lib.org/)
  * [tidyverse](https://www.tidyverse.org/packages/)
  * [utils](https://cran.r-project.org/web/packages/R.utils/index.html) for 
  working with files
* [PostGIS](https://PostGIS.net/) for the national dataset
