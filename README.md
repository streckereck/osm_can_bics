# OSM Can-BICS
A national map of bicycle facilities using meaningful labels.

![Bicycle facilities across Canada](figures/glow.png?raw=true "Bicycle facilities across Canada")

## Table of contents

* [Introduction](#introduction)
* [Technology](#technology)
* [Sample cities](#sample-cities)
* [National map](#national-map)
* [Change detection](#future-change-detection) (future work)

## Introduction
OSM Can-BICS classifies [OpenStreetMap (OSM)](https://www.openstreetmap.org/) to create a network dataset of bicycle facilities, classified according to the [Canadian Bikeway Comfort and Safety Classification System (Can-BICS)](https://www.canada.ca/en/public-health/services/reports-publications/health-promotion-chronic-disease-prevention-canada-research-policy-practice/vol-40-no-9-2020/canbics-classification-system-naming-convention-cycling-infrastructure.html). 

To create bicycling facility datasets, many studies assemble open data provided by cities. However, compiling open data sources leads to inconsistent labeling of facilities across cities, and many studies do not provide a statement of accuracy. OpenStreetMap provides a more collaborative approach through a central repository and benefits from community contributors with on-the-ground knowledge. OSM Can-BICS provides a research ready dataset complete with measures of bias and accuracy, and a well-defined path for future change detection studies.

We developed OSM-Can-BICS using the following three steps:

1. Analyse [sample cities](#sample-cities) to develop the classification and perform an accuracy assessment.
2. Create the [national network dataset](#national-dataset).
3. Change detection (future work).

## Technology

* [R](https://www.r-project.org/) and [R Studio](https://www.rstudio.com/)
* R Packages:
  * [lwgeom](https://cran.r-project.org/web/packages/lwgeom/index.html)
  * [mapview](https://r-spatial.github.io/mapview/)
  * [osmdata](https://github.com/ropensci/osmdata) for the sample cities
  * [raster](https://cran.r-project.org/web/packages/raster/index.html)
  * [sf](https://r-spatial.github.io/sf/)
  * [tidyverse](https://www.tidyverse.org/packages/)
* [PostGIS](https://PostGIS.net/) for the national dataset

## Sample cities
15 cities, spanning across Canada, were selected and stratified by population (small < 50,000; medium 50,000 to 500,000; and large > 500,000). Both the reference data and OSM data were acquired in the summer of 2020.

### Reference data
We collected more than 2000 reference points using street level imagery, aerial imagery, and other reference data where available (e.g. newspaper articles about new projects, pdf maps provided by cities, and local knowledge). 

We collected a stratified random sample at the following rates:

* 1 sample per 10 km for low and medium comfort infrastructure (painted lanes and multi-use paths).
* 1 sample per km for high comfort infrastructure (bike paths, cycle tracks, and local street bikeways) (high comfort facilities are relatively uncommon and are important for cycling safety and preference). 
* 1 sample per km for facilities that are on OSM, but not open data (to provide greater scrutiny). 

The reference data were randomly split into training (70%) and testing (30%). Four interpreters used the [interpretation guidelines](https://docs.google.com/document/d/1M5wUzod1OPEfSOnpgzl4S9QM92IbeKEwJr7UVeX_zn8/edit?usp=sharing) to assign Can-BICS labels. Interpreters marked locations for later review, where necessary.

### OSM Data
OSM data (OpenStreetMap Contributors, 2021) were downloaded for the 15 sample cities in the summer of 2020 (to match the reference data) using the query `highway = *` in the R package [osmdata](https://cran.r-project.org/web/packages/osmdata/index.html).

### Data acquisition
Two additional steps are needed to download large files for the 15 sample cities:
1. Download the [reference and OSM data](https://www.dropbox.com/s/bd6tjq0rhznfa90/sample_cities.zip?dl=0) and extract to `data/sample_cities/` (65 MB)
2. Download [landcover data for Canada](https://ftp.maps.canada.ca/pub/nrcan_rncan/Land-cover_Couverture-du-sol/canada-landcover_canada-couverture-du-sol/CanadaLandcover2015.zip) (2 GB), store and unpack where convenient, and update the path in `code/sample_cities/classify.R`

### Processing
1. `/code/sample_cities/classify.R` classifies OSM data for the 15 sample cities.
  * functions to classify OSM data are located in the file `/code/Can_BICS_OSM_classify.R`. This file can be modified for different research goals.
  * supporting functions are located in `/code/Can_BICS_OSM_functions.R`
2. `/code/sample_cities/overall_accuracy.RMD` generates an accuracy assessment. Requires that classify.R has been run.

![Classification algorithm](figures/classify.png?raw=true "Classification algorithm")
**Figure 1** Classification algorithm.

## National dataset