# Evaluating differential air quality impacts of prescribed fire and wildfire
Chang T-H, Qiu M, Wei Y, Wu X (2025).

## Data processing
1. Download the following data:
  - MTBS Burned Areas Boundaries Dataset [(source)](https://www.mtbs.gov/direct-download)
  - MTBS Burn Severity Mosaics for CA, FL, GA [(source)](https://www.mtbs.gov/direct-download) - select state (CA, FL, GA) and year (2006 - 2020), enter email to download zip files
  - Globfire [(source)](https://www.dropbox.com/scl/fi/d7wuk2csy6u14tq36d0hs/globfire_link_dataset_Oct10_2023.zip?rlkey=0inyr19tnaggy97t9sg54nh22&e=2&dl=0)
  - State bounds [(source)](https://www2.census.gov/geo/tiger/GENZ2020/shp/) - download "cb_2020_us_state_500k.zip"
  - Topography [(source)](https://www.earthenv.org/topography) - select dataset (Elevation, Slope, Aspect Cosine, Aspect Sine), aggregation (Median), sources (GMTED2010), resolution (1KM)
  - Land cover [(source)](https://www.mrlc.gov/data)
  - Climate [(source)](https://www.northwestknowledge.net/metdata/data/) - download files for variable ("pr", "th", "vs", "vpd", "tmmn", "tmmx", "rmin", "rmax") and year (2006 - 2020)
  - Smoke exposure can be found in `processed_data/sorted_total_smokepm_dt.RDS`, which I generated from [GitHub Repo](https://github.com/jeffwen/smoke_linking_public)
2. Create folder `raw_data` and include the following subfolders:
  - `mtbs_perimeter_data`
  - `MTBS_BSmosaics_CA`, `MTBS_BSmosaics_FL`, `MTBS_BSmosaics_GA` (each of these should contain a folder for each year from 2006 to 2020)
  - `globfire_link_dataset_Oct10_2023`
  - `cb_2020_us_state_500k`
  - `topography` (put the downloaded topography .tif files here)
  - `NLCD_landcover_2021_release_all_files_20230630`
  - `climate` (put the downloaded climate .nc files here)
4. Create folder `processed_data` to store all processed data. Make sure to add the smoke data `sorted_total_smokepm_dt.RDS` as well. 
3. Run `code/data_processing.R`. 

## Analysis
1. Make sure the following data are available:
  - `processed_data/dat.RDS` (fire observations linked to topography, land cover, climate, and smoke)
  - `processed_data/fire_dat.RDS` (fire observations with geometry)
  - `raw_data/cb_2020_us_state_500k/cb_2020_us_state_500k.shp` (state bounds)
2. Run `code/analysis.R` to perform the analyses in the manuscript and generate figures. 
