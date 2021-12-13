# OSM Can-BICS
A national bicycle infrastructure dataset from OpenStreetMap with consistent 
labels.

![Bicycle facilities across Canada](figures/glow.png?raw=true "Bicycle facilities across Canada")

## Table of contents

* [Introduction](#introduction)
* [Technology](#technology)
* [Test cities](#test-cities)
* [National dataset](#national-dataset)

## Introduction
OSM Can-BICS classifies [OpenStreetMap (OSM)](https://www.openstreetmap.org/) to 
create a network dataset of bicycle facilities, classified according to the 
[Canadian Bikeway Comfort and Safety Classification System (Can-BICS)](https://www.canada.ca/en/public-health/services/reports-publications/health-promotion-chronic-disease-prevention-canada-research-policy-practice/vol-40-no-9-2020/canbics-classification-system-naming-convention-cycling-infrastructure.html). 

To create bicycling facility datasets, many studies assemble open data provided 
by cities. However, compiling open data sources leads to inconsistent labeling 
of facilities across cities, and many studies do not provide a statement of 
accuracy. OpenStreetMap provides a more collaborative approach through a central 
repository and benefits from community contributors with on-the-ground 
knowledge. OSM Can-BICS provides a research ready dataset complete with measures 
of bias and accuracy in 15 test cities, and a well-defined path for future change 
detection studies.

We developed OSM-Can-BICS using the following steps:

1. Process [test cities](#test-cities) to develop the classification and 
perform an accuracy assessment.
2. Create the [national network dataset](#national-dataset).

The code and dataset are under development. Our preliminary national dataset is 
available from [ArcGIS online](https://arcg.is/0eyGy9).

## Technology

* [R](https://www.r-project.org/) and [R Studio](https://www.rstudio.com/)
* R Packages:
  * [cancensus](https://mountainmath.github.io/cancensus/index.html)
  * [ggridges](https://www.rdocumentation.org/packages/ggridges/versions/0.5.3)
  * [janitor](https://www.rdocumentation.org/packages/janitor/versions/2.1.0)
  * [lwgeom](https://cran.r-project.org/web/packages/lwgeom/index.html)
  * [mapview](https://r-spatial.github.io/mapview/)
  * [osmdata](https://github.com/ropensci/osmdata) for the test cities
  * [raster](https://cran.r-project.org/web/packages/raster/index.html)
  * [RPostgreSQL](https://cran.r-project.org/web/packages/RPostgreSQL/index.html) for the national dataset
  * [sf](https://r-spatial.github.io/sf/)
  * [testthat](https://testthat.r-lib.org/)
  * [tidyverse](https://www.tidyverse.org/packages/)
  * [utils](https://cran.r-project.org/web/packages/R.utils/index.html) for 
  working with files
* [PostGIS](https://PostGIS.net/) for the national dataset

## Test cities
We selected 15 cities spanning across Canada, stratified by population 
(small < 50,000; medium 50,000 to 500,000; and large > 500,000) to 
collect reference data and perform an accuracy assessment. Both the 
reference data and OSM data were acquired in the summer of 2020.

### Reference data
We collected more than 2000 reference points using street level imagery, aerial 
imagery, and other reference data where available (e.g. newspaper articles about 
new projects, pdf maps provided by cities, and local knowledge). 

We collected a stratified random sample at the following rates:

* 1 sample per 10 km for low and medium comfort infrastructure (painted lanes 
and multi-use paths).
* 1 sample per km for high comfort infrastructure (bike paths, cycle tracks, and 
local street bikeways) (high comfort facilities are relatively uncommon and are 
important for cycling safety and preference). 
* 1 sample per km for facilities that are on OSM, but not open data (to provide 
greater scrutiny). 

The reference data were randomly split into training (70%) and testing (30%). 
Four interpreters used the [interpretation guidelines](https://docs.google.com/document/d/1M5wUzod1OPEfSOnpgzl4S9QM92IbeKEwJr7UVeX_zn8/edit?usp=sharing) 
to assign Can-BICS labels. Interpreters marked locations for later review, 
where necessary.

### OSM Data
OSM data (OpenStreetMap Contributors, 2021) were downloaded for the 15 test 
cities in the summer of 2020 (to match the reference data) using the query 
`highway = *` in the R package [osmdata](https://cran.r-project.org/web/packages/osmdata/index.html).

### Data acquisition for processing
Two additional steps are needed to download large files:
1. Download the [reference and OSM data](https://www.dropbox.com/s/bd6tjq0rhznfa90/sample_cities.zip?dl=0) and extract to `data/test_cities/` (65 MB). (data available soon)
2. Download [landcover data for Canada](https://ftp.maps.canada.ca/pub/nrcan_rncan/Land-cover_Couverture-du-sol/canada-landcover_canada-couverture-du-sol/CanadaLandcover2015.zip) (2 GB), store and unpack where convenient, and update the path in `/code/paths_and_variables.R`

### Processing
1. `/code/test_cities/classify.R` classifies OSM data for the 15 test cities.
  * functions to classify OSM data are located in the file `/code/Can_BICS_OSM_classify.R`. 
  This file can be modified for different research goals.
  * supporting functions are located in `/code/Can_BICS_OSM_functions.R`
2. `/code/test_cities/overall_accuracy.RMD` generates an accuracy assessment. 
Requires that classify.R has been run.

![Classification algorithm](figures/classify.png?raw=true "Classification algorithm")
**Figure 1** Classification algorithm.

## National dataset
The national dataset uses data from 
[Geofabrk](https://www.geofabrik.de/data/) and [PostGIS](https://postgis.net/) for 
storage, due to the large extent. Processing is done province-by-province, and 
the processed data is combined into the national dataset.

### Database setup
Due to the volume of data, we use PostGIS to store and manage the data. These 
steps were tested on Windows 10, but should be adaptable to other platforms.
1. Install [PostGIS](https://postgis.net/windows_downloads/). 
2. Install [OSM2PSQL](https://osm2pgsql.org/doc/install.html#installing-on-windows)
3. Create a default account called "postgres" with no password when executed 
from a local environment (careful in network accessible environments!).

### Data acquisition
Three additional steps are needed to download large files:
1. Download the OSM data by running `/code/national/download_data.R`. 
2. Download [landcover data for Canada](https://ftp.maps.canada.ca/pub/nrcan_rncan/Land-cover_Couverture-du-sol/canada-landcover_canada-couverture-du-sol/CanadaLandcover2015.zip) (2 GB), store and unpack where convenient, and update the path in `/code/paths_and_variables.R`
3. Download [Canada Census Subdivision Boundaries](https://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/bound-limit-2016-eng.cfm), 
store and unpack where convenient, and update the path in `/code/paths_and_variables.R`

### Processing
1. `/code/national/classify.R` classifies OSM data for the provinces.
  * functions to classify OSM data are located in the file `/code/Can_BICS_OSM_classify.R`. 
  * supporting functions are located in `/code/Can_BICS_OSM_functions.R`
  * you can subset the data and run multiple simultaneous classifications.
2. `/code/national/export_data.R` exports data into shapefile and json formats.
3. `/code/national/reporting.RMD` generates summary statistics. 
Requires that `/code/national/classify.R` and `/code/national/export_data.R` has been run.
