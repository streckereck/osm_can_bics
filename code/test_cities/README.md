# Test cities
The project introduction is [here](../../README.md).
Building a national network dataset is [here](../../code/national).


We selected 15 cities spanning across Canada, stratified by population 
(small < 50,000; medium 50,000 to 500,000; and large > 500,000) to 
collect reference data and perform an accuracy assessment of our classification 
of OSM data using Can-BICS labels. Both the reference data and OSM data were 
acquired in the summer of 2020.

### Reference data
We collected more than 2000 reference points using street level imagery, aerial 
imagery, and other reference data where available (e.g. newspaper articles about 
new projects, pdf maps provided by cities, and local knowledge). 

We collected a stratified random sample at the following rates:

* 1 sample per 10 km for low and medium comfort infrastructure (painted lanes 
and multi-use paths).
* 1 sample per km for high comfort infrastructure (bike paths, cycle tracks, and 
local street bikeways) (high comfort infrastructure are relatively uncommon and 
are important for cycling safety and preference). 
* 1 sample per km for infrastructure that is on OSM, but not open data (to 
provide greater scrutiny). 

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
1. Download the [reference and OSM data](https://www.dropbox.com/s/bd6tjq0rhznfa90/test_cities.zip?dl=0) and extract to `data/test_cities/` (65 MB).
2. Download [landcover data for Canada](https://ftp.maps.canada.ca/pub/nrcan_rncan/Land-cover_Couverture-du-sol/canada-landcover_canada-couverture-du-sol/CanadaLandcover2015.zip) (2 GB), store and unpack where 
convenient, and update the path in `/code/paths_and_variables.R`

### Processing
1. `/code/test_cities/classify.R` classifies OSM data for the 15 test cities.
  * functions to classify OSM data are located in the file `/code/Can_BICS_OSM_classify.R`. 
  This file can be modified for different research goals.
  * supporting functions are located in `/code/Can_BICS_OSM_functions.R`
2. `/code/test_cities/overall_accuracy.RMD` generates an accuracy assessment. 
Requires that classify.R has been run.
