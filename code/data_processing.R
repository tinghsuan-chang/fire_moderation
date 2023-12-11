# load packages ------------------------------------------------------
library(sf)
library(tidyverse)
library(raster)
library(terra)
library(lubridate)

# prepare MTBS and GlobFire data -------------------------------------
# MTBS description:
# https://developers.google.com/earth-engine/datasets/catalog/USFS_GTAC_MTBS_burned_area_boundaries_v1

# MTBS Burned Areas Boundaries Dataset
mtbs_bab_raw <- st_read("raw_data/mtbs_perimeter_data/mtbs_perims_DD.shp")
llprj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
mtbs_bab <- mtbs_bab_raw %>%
  st_transform(crs = llprj) %>%
  mutate(state = substr(Event_ID, 1, 2)) %>%
  filter(state %in% c("CA", "FL", "GA")) %>%
  filter(Ig_Date >= "2006-01-01" & Ig_Date <= "2020-12-31") %>%
  mutate(burn_severity_max = case_when(!(High_T %in% c(-9999, 9999, -999, 999)) ~ "High",
                                       !(Mod_T %in% c(-9999, 9999, -999, 999)) ~ "Moderate",
                                       !(Low_T %in% c(-9999, 9999, -999, 999)) ~ "Low",
                                       !(IncGreen_T %in% c(-9999, 9999, -999, 999)) ~ "Increased Greeness / Unchanged"))

# globfire dataset of all fires that are able to link to smoke. 
globfire_daily_raw <- readRDS("raw_data/globfire_link_dataset_Oct10_2023/daily_globfire_link.rds")
# polygon shapefile for the fires that are documented in the globfire dataset
globfire_poly_raw <- st_read("raw_data/globfire_link_dataset_Oct10_2023/globfire_na_final_area_2006-2020.shp")
# transform the globfire polygon shapefile so that it has the same coordinate reference system as mtbs
globfire_poly <- st_transform(globfire_poly_raw, crs = st_crs(mtbs_bab))

# join MTBS and GlobFire ---------------------------------------------------
sf_use_s2(FALSE)
# return MTBS features augmented with the fields of GlobFire that have the largest overlap with each of the features of MTBS
fire_dat <- st_join(mtbs_bab, globfire_poly,
                    join = st_intersects,
                    largest = TRUE) 
# calculate intersecting area
intersection <- st_intersection(mtbs_bab, globfire_poly) %>% 
  mutate(intersect_area = st_area(.)) %>%
  dplyr::select(Event_ID, Id, intersect_area) %>%
  st_drop_geometry()
fire_dat <- fire_dat %>%
  mutate(polygon_area = st_area(fire_dat),
         diff_time = abs(difftime(Ig_Date, IDate, units = "days"))) %>%
  # spatially matched fires must happen within 30 days of each other 
  filter(diff_time <= 30) %>%
  left_join(intersection, by = c("Event_ID", "Id")) %>%
  # calculate intersecting area / MTBS area
  mutate(intersect_pct = intersect_area*100 / polygon_area)

# non-unique matches
dup <- fire_dat %>%
  filter(Id %in% fire_dat$Id[duplicated(fire_dat$Id)]) %>%
  arrange(Id, diff_time)
# for non-unique matches, only keep the MTBS event whose Ig_Date is closest to the GlobFire IDate
dup_closest <- dup %>%
  group_by(Id) %>%
  top_n(-1, diff_time) %>%
  distinct(Id, .keep_all = TRUE)
dup_remove_event_id <- dup$Event_ID[!(dup$Event_ID %in% dup_closest$Event_ID)] 
fire_dat <- fire_dat %>%
  filter(!(Event_ID %in% dup_remove_event_id))

summary(fire_dat$intersect_pct) # Min. 0.078, 1st Qu. 39.35, Median 63.66, Mean 59.04, 3rd Qu. 80.8, Max. 98.6
# keep matches with intersecting area > 39% 
fire_dat <- fire_dat %>%
  filter(as.numeric(intersect_pct) > 39)
#saveRDS(fire_dat, file = "processed_data/fire_dat.RDS")

# burn severity ----------------------------------------------------------
#fire_dat <- readRDS(file = "processed_data/fire_dat.RDS")
# function to get burn severity classes in the boundaries for each fire in a specific state and year
get_value_inboundaries <- function(location, fire_year) {
  mtbs_rast <- terra::rast(file.path("raw_data",
                                     paste0("mtbs_", location),
                                     paste0("mtbs_", location, "_", fire_year),
                                     paste0("mtbs_", location, "_", fire_year, ".tif")))
  mtbs_rast <- terra::project(mtbs_rast, llprj, method = "near")
  d <- subset(fire_dat, format(fire_dat$Ig_Date, "%Y") == fire_year & state == location)
  value_inboundaries <- terra::extract(mtbs_rast, vect(d), na.rm = TRUE)
  return(value_inboundaries)
}

# function to get the mode for a vector 
get_mode <- function(v) {
  valid_values <- na.omit(v)
  uniqv <- unique(valid_values)
  uniqv[which.max(tabulate(match(valid_values, uniqv)))]
}

# get the most common burn severity class in the boundaries for each fire
for (location in c("CA", "FL", "GA")) {
  for (fire_year in as.character(2006:2020)) {
    d <- subset(fire_dat, format(fire_dat$Ig_Date, "%Y") == fire_year & state == location)
    if (nrow(d) == 0) next
    value_inboundaries <- get_value_inboundaries(location, fire_year)
    mode_inboundaries <- value_inboundaries %>%
      group_by(ID) %>%
      summarise(mode = get_mode(eval(parse(text = paste0("mtbs_", location, "_", fire_year)))))
    d <- d %>%
      dplyr::select(Event_ID) %>%
      cbind(mode_inboundaries) %>%
      rename(burn_severity_mode = mode) %>%
      dplyr::select(!ID)
    print(paste0(location, fire_year))
    saveRDS(d, file = paste0("processed_data/burn_severity_dat/", location, fire_year, ".RDS"))
  }
}

# combine burn severity data
burn_severity_dat <- list.files( path = "processed_data/burn_severity_dat/", pattern = "*.RDS", full.names = TRUE ) %>%
  map_dfr(readRDS)
#saveRDS(burn_severity_dat, file = "processed_data/burn_severity_dat.RDS")
prop.table(table(burn_severity_dat$burn_severity_mode)) # Unburned-Low 3.5%, Low 76.5%, Mod 14.5%, High 3.1%, Missing 2.2%

# topography data --------------------------------------------------------
# get state bounds
obj <- st_read("raw_data/cb_2020_us_state_500k/cb_2020_us_state_500k.shp")
CA_bound <- subset(obj, NAME == "California") %>% st_transform(crs = 4326)
FL_bound <- subset(obj, NAME == "Florida") %>% st_transform(crs = 4326)
GA_bound <- subset(obj, NAME == "Georgia") %>% st_transform(crs = 4326)

# elevation
elev <- raster("raw_data/topography/elevation_1KMmd_GMTEDmd.tif")
elev_grid_ca_poly <- raster::extract(crop(elev, extent(CA_bound)), fire_dat %>% filter(state == "CA"), 
                                     fun = mean, na.rm = TRUE)
elev_grid_fl_poly <- raster::extract(crop(elev, extent(FL_bound)), fire_dat %>% filter(state == "FL"), 
                                     fun = mean, na.rm = TRUE)
elev_grid_ga_poly <- raster::extract(crop(elev, extent(GA_bound)), fire_dat %>% filter(state == "GA"), 
                                     fun = mean, na.rm = TRUE)

# slope
slope <- raster("raw_data/topography/slope_1KMmd_GMTEDmd.tif")
slope_grid_ca_poly <- raster::extract(crop(slope, extent(CA_bound)), fire_dat %>% filter(state == "CA"), 
                                      fun = mean, na.rm = TRUE)
slope_grid_fl_poly <- raster::extract(crop(slope, extent(FL_bound)), fire_dat %>% filter(state == "FL"), 
                                      fun = mean, na.rm = TRUE)
slope_grid_ga_poly <- raster::extract(crop(slope, extent(GA_bound)), fire_dat %>% filter(state == "GA"), 
                                      fun = mean, na.rm = TRUE)

# aspect
aspect_sin <- raster("raw_data/topography/aspectsine_1KMmd_GMTEDmd.tif")
aspectsin_grid_ca_poly <- raster::extract(crop(aspect_sin, extent(CA_bound)), fire_dat %>% filter(state == "CA"), 
                                          fun = mean, na.rm = TRUE)
aspectsin_grid_fl_poly <- raster::extract(crop(aspect_sin, extent(FL_bound)), fire_dat %>% filter(state == "FL"), 
                                          fun = mean, na.rm = TRUE)
aspectsin_grid_ga_poly <- raster::extract(crop(aspect_sin, extent(GA_bound)), fire_dat %>% filter(state == "GA"), 
                                          fun = mean, na.rm = TRUE)
aspect_cos <- raster("raw_data/topography/aspectcosine_1KMmd_GMTEDmd.tif")
aspectcos_grid_ca_poly <- raster::extract(crop(aspect_cos, extent(CA_bound)), fire_dat %>% filter(state == "CA"), 
                                          fun = mean, na.rm = TRUE)
aspectcos_grid_fl_poly <- raster::extract(crop(aspect_cos, extent(FL_bound)), fire_dat %>% filter(state == "FL"), 
                                          fun = mean, na.rm = TRUE)
aspectcos_grid_ga_poly <- raster::extract(crop(aspect_cos, extent(GA_bound)), fire_dat %>% filter(state == "GA"), 
                                          fun = mean, na.rm = TRUE)

# combine topography data
elev_grid_poly <- rbind(elev_grid_ca_poly, elev_grid_fl_poly, elev_grid_ga_poly) 
slope_grid_poly <- rbind(slope_grid_ca_poly, slope_grid_fl_poly, slope_grid_ga_poly)
aspectsin_grid_poly <- rbind(aspectsin_grid_ca_poly, aspectsin_grid_fl_poly, aspectsin_grid_ga_poly)
aspectcos_grid_poly <- rbind(aspectcos_grid_ca_poly, aspectcos_grid_fl_poly, aspectcos_grid_ga_poly)
topography_grid_poly <- cbind(elev_grid_poly, slope_grid_poly, aspectsin_grid_poly, aspectcos_grid_poly)
colnames(topography_grid_poly) <- c("elevation", "slope", "aspect_sin", "aspect_cos")
topography_dat <- rbind(dat %>% filter(state == "CA"), dat %>% filter(state == "FL"), dat %>% filter(state == "GA")) %>%
  dplyr::select(Event_ID) %>%
  cbind(topography_grid_poly)
#saveRDS(topography_dat, file = "processed_data/topography_dat.RDS")

# landcover data ----------------------------------------------------------
# data source: https://www.mrlc.gov/data/legends/national-land-cover-database-class-legend-and-description
# use the closest year when the fire year is not in the landcover data source
landcover_years <- c(2006, 2008, 2011, 2013, 2016, 2019)
fire_dat <- fire_dat %>%
  mutate(landcover_yr = case_when(year(Ig_Date) %in% landcover_years ~ format(Ig_Date, "%Y"),
                                  Ig_Date >= "2007-01-01" & Ig_Date <= "2007-06-30" ~ "2006",
                                  Ig_Date >= "2007-07-01" & Ig_Date <= "2007-12-31" ~ "2008",
                                  year(Ig_Date) == 2009 ~ "2008",
                                  year(Ig_Date) == 2010 ~ "2011",
                                  Ig_Date >= "2012-01-01" & Ig_Date <= "2012-06-30" ~ "2011",
                                  Ig_Date >= "2012-07-01" & Ig_Date <= "2012-12-31" ~ "2013",
                                  year(Ig_Date) == 2014 ~ "2013",
                                  year(Ig_Date) == 2015 ~ "2016",
                                  year(Ig_Date) == 2017 ~ "2016",
                                  year(Ig_Date) == 2018 ~ "2019",
                                  year(Ig_Date) == 2020 ~ "2019"))
for (year in as.character(c(2006, 2008, 2011, 2013, 2016, 2019))) {
  land_rast <- terra::rast(file.path("raw_data/landcover/NLCD_landcover_2021_release_all_files_20230630",
                                     paste0("nlcd_", year, "_land_cover_l48_20210604.img")))
  land_rast <- project(land_rast, llprj, method = "near")
  d <- fire_dat %>% filter(landcover_yr == year)
  class_inboundaries <- terra::extract(land_rast, vect(d), na.rm = TRUE)
  class_pct_inboundaries <- class_inboundaries %>%
    group_by(ID) %>%
    summarise(forest = mean(`NLCD Land Cover Class` %in% c("Deciduous Forest", "Evergreen Forest", "Mixed Forest")),
              shrubland = mean(`NLCD Land Cover Class` %in% c("Dwarf Scrub", "Shrub/Scrub")),
              herb = mean(`NLCD Land Cover Class` == "Herbaceous")) %>%
    mutate(other = 1 - (forest + shrubland + herb))
  d <- d %>%
    dplyr::select(Event_ID) %>%
    cbind(class_pct_inboundaries) %>%
    dplyr::select(!ID)
  saveRDS(d, file = paste0("processed_data/landcover_dat/", paste0("landcover_dat", year), ".RDS"))
}

# combine landcover data
landcover_dat <- list.files( path = "processed_data/landcover_dat/", pattern = "*.RDS", full.names = TRUE ) %>%
  map_dfr(readRDS)
#saveRDS(landcover_dat, file = "processed_data/landcover_dat.RDS")

# climate data ------------------------------------------------------------
# data source: https://developers.google.com/earth-engine/datasets/catalog/IDAHO_EPSCOR_GRIDMET
# climate vars: precipitation, wind direc, wind veloc, pressure, min temp, max temp, min rel humidity, max rel humidity
climate_vars <- c("pr", "th", "vs", "vpd", "tmmn", "tmmx", "rmin", "rmax") 
d <- fire_dat %>% st_drop_geometry()
sf_use_s2(FALSE)
for (v in 1:len(climate_vars)) {
  means <- c()
  for (i in 1:nrow(d)) {
    yr <- year(d[i,"Ig_Date"])
    day_in_yr <- yday(d[i,"Ig_Date"])
    stack <- stack(paste0("raw_data/climate/", climate_vars[v], "_", as.character(yr), ".nc"))
    layer_climate <- data.frame(rasterToPoints((stack[[day_in_yr]])))
    colnames(layer_climate)[1:3] <- c("LONGITUDE", "LATITUDE", climate_vars[v])
    layer_climate <- st_as_sf(layer_climate,
                              coords = c("LONGITUDE", "LATITUDE"),
                              crs = 4326,
                              remove = FALSE)
    layer_climate_gridded <- st_drop_geometry(st_join(fire_dat[i,], layer_climate, join = st_intersects))
    means <- c(means, mean(layer_climate_gridded[,climate_vars[v]], na.rm = TRUE))
  }
  d$v <- means
  colnames(d)[ncol(d)] <- climate_vars[v]
}
climate_dat <- d[,c("Event_ID", climate_vars)]
saveRDS(climate_dat, file = "processed_data/climate_dat.RDS")


