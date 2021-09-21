# OSM Can-BICS
A national map of bicycle facilities using meaningful labels.

![Bicycle facilities across Canada](figures/glow.png?raw=true "Bicycle facilities across Canada")

## Table of contents

* [Introduction](#introduction)
* [Technology](#technology)
* [Sample cities](#sample-cities)
* [National map](#national-map)
* [Future change detection](#future-change-detection)

## Introduction
OSM Can-BICS classifies [OpenStreetMap (OSM)](https://www.openstreetmap.org/) data, according to the [Canadian Bikeway Comfort and Safety Classification System (Can-BICS)](https://www.canada.ca/en/public-health/services/reports-publications/health-promotion-chronic-disease-prevention-canada-research-policy-practice/vol-40-no-9-2020/canbics-classification-system-naming-convention-cycling-infrastructure.html). Many research studies assemble open data provided by multiple cities, repeating effort, using inconsistent labels, and without providing a statement of accuracy. OpenStreetMap provides a more collaborative approach through a central repository and benefits from community contributors with on-the-ground knowledge. OSM Can-BICS provides a research ready dataset complete with measures of bias and accuracy, and a well defined path for future change detection studies.

There are three main sections:

1. Analyse [sample cities](#sample-cities) to develop the classification and perform an accuracy assessment.
2. Process the [national dataset](#national-dataset).

## Technology

* [R](https://www.r-project.org/) and [R Studio](https://www.rstudio.com/)
* Simple features using the R package [sf](https://r-spatial.github.io/sf/) for spatial data processing
* The R package [osmdata](https://github.com/ropensci/osmdata) to download osm data for the 15 sample cities
* [PostGIS](https://PostGIS.net/) to manage the national dataset

## Sample cities
15 Sample cities were selected, stratified by population (small < 50,000; medium 50,000 to 500,000; and large > 500,000), and spanning across Canada. Both the reference data and OSM data were acquired in the summer of 2020.

### Reference data
We collected more than 2000 reference points using street level imagery, aerial imagery, and other reference data where available. We collected a stratified random sample at a rate of 1 sample per 10 km for low and medium comfort infrastructure (painted lanes and multi-use paths), and 1 sample per km for high comfort infrastructure (bike paths, cycle tracks, and local street bikeways) and facilities that are on OSM, but not open data. The reference data were randomly split into training (70%) and testing (30%). Four interpreters used the [intretation guidelines](https://docs.google.com/document/d/1M5wUzod1OPEfSOnpgzl4S9QM92IbeKEwJr7UVeX_zn8/edit?usp=sharing) to assign Can-BICS labels. Interpreters marked locations for later review, where necessary.

### OSM Data
OSM data (OpenStreetMap Contributors, 2021) were downloaded for the 15 sample cities in the summer of 2020 (to match the reference data) using the query `highway = *` in the R package [osmdata](https://cran.r-project.org/web/packages/osmdata/index.html).

### Data acquisition
Two additional steps are needed to download large files for the 15 sample cities:
1. Download the [reference and OSM data](HERE) and unpack them in the folder `data/sample_cities/original/` (150 MB)
2. Download [landcover data for Canada](https://ftp.maps.canada.ca/pub/nrcan_rncan/Land-cover_Couverture-du-sol/canada-landcover_canada-couverture-du-sol/CanadaLandcover2015.zip) (2 GB), store and unpack where convenient, and update the path in `code/sample_cities/classify.R`

### Processing
1. `/code/sample_cities/classify.R` classifies OSM data for the 15 sample cities.
  * functions to classify OSM data are located in the file `/code/Can_BICS_OSM_classify.R`, which can be modified.
  * supporting functions are located in `/code/Can_BICS_OSM_functions.R`
2. `/code/sample_cities/overall_accuracy.RMD` generates an accuracy assessment. Requires that classify.R has been run.

![Classification algorithm](figures/classify.png?raw=true "Classification algorithm")
**Figure 1** Classification algorithm.

## National dataset