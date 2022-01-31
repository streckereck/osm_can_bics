# Create a national dataset
**The processed national network dataset is available on 
[ArcGIS online](https://arcg.is/0PyqOu).**

The project introduction is [here](../../).

The national network dataset is a classification of OpenStreetMap data using 
consistent lables from the 
[Canadian Bikeway Comfort and Safety Classification System (Can-BICS)](https://www.canada.ca/en/public-health/services/reports-publications/health-promotion-chronic-disease-prevention-canada-research-policy-practice/vol-40-no-9-2020/canbics-classification-system-naming-convention-cycling-infrastructure.html).

The national dataset uses data from 
[Geofabrk](https://www.geofabrik.de/data/) and [PostGIS](https://postgis.net/) 
for storage, due to the large extent. Processing is done province-by-province, 
and the processed data is combined into the national dataset.

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
  * you can subset the data and run multiple simultaneous/parallel 
  classifications (for efficiency).
  * In testing, processing took approximately 1 weekend to run for the national 
  dataset (start on Friday afternoon, ready for Monday morning).
2. `/code/national/export_data.R` exports data into shapefile and json formats.
3. `/code/national/reporting.RMD` generates summary statistics. 
Requires that `/code/national/classify.R` and `/code/national/export_data.R` 
have been run.

![Classification algorithm](../../figures/classify.png?raw=true "Classification algorithm")
**Figure 1** Classification algorithm.
