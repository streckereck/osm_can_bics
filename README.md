# OSM Can-BICS
![Bicycle facilities across Canada](figures/glow.png?raw=true "Bicycle facilities across Canada")

OSM Can-BICS classifies [OpenStreetMap (OSM)](https://www.openstreetmap.org/) data, according to the [Canadian Bikeway Comfort and Safety Classification System (Can-BICS)](https://www.canada.ca/en/public-health/services/reports-publications/health-promotion-chronic-disease-prevention-canada-research-policy-practice/vol-40-no-9-2020/canbics-classification-system-naming-convention-cycling-infrastructure.html). The goal is to create a national map of bicycling facilities that is up-to-date, detailed, and uses lables that are meaningful for cycling safety and preference research.

There are two main sections:
1. Analyse sample cities to develop the classification and perform an accuracy assessment.
2. Process the national dataset.

## Sample cities
15 Sample cities were selected, stratified by population (small < 50,000; medium 50,000 to 500,000; and large > 500,000), and spanning accross Canada. Both the reference data and OSM data were acquired in the summer of 2020.

### Reference data
We collected more than 2000 reference points using street level imagery, aerial imagery, and other reference data where needed. The reference data are randomly split into training (70%) and testing (30%).

### OSM Data
OSM data (OpenStreetMap Contributors, 2021) were downloaded for the 15 sample cities in the summer of 2020 (to match the reference data) using the query `highway = *` in the R package [osmdata](https://cran.r-project.org/web/packages/osmdata/index.html).

### Data acquisition
Two additional steps are needed to download large files for the 15 sample cities:
1. Download the [reference and OSM data](HERE) and unpack them in the folder `data/sample_cities/original/` (150 MB)
2. Download [landcover data for Canada](https://ftp.maps.canada.ca/pub/nrcan_rncan/Land-cover_Couverture-du-sol/canada-landcover_canada-couverture-du-sol/CanadaLandcover2015.zip) (2 GB), store and unpack where convenient, and update the path in `code/sample_cities/classify.R`

### Processing
1. `/code/sample_cities/classify.R` classifies OSM data for the 15 sample cities.
  * functions to classify OSM data are located in the file `/code/Can_BICS_OSM_classify.R`, which can be modified depending on your analsis goals.
  * supporting functions are locaed in `/code/Can_BICS_OSM_functions.R`
2. `/code/sample_cities/overall_accuracy.RMD` generates an accuracy assessment. Requres that classify.R has been run.

![Classification algorithm](figures/classify.png?raw=true "Classification algorithm")
**Figure 1** Classification algorithm.

## National dataset