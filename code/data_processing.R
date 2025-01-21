# Load packages ------------------------------------------------------
library(sf)
library(tidyverse)
library(raster)
library(terra)
library(lubridate)
library(fst)

# Prepare MTBS and GlobFire data -------------------------------------
# MTBS description:
# https://developers.google.com/earth-engine/datasets/catalog/USFS_GTAC_MTBS_burned_area_boundaries_v1

# MTBS Burned Areas Boundaries Dataset
llprj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
mtbs_bab_raw <- st_read("raw_data/mtbs_perimeter_data/mtbs_perims_DD.shp")
mtbs_bab <- mtbs_bab_raw %>%
  st_transform(crs = llprj) %>%
  mutate(state = substr(Event_ID, 1, 2)) %>%
  filter(state %in% c("CA", "FL", "GA")) %>%
  filter(Ig_Date >= "2006-01-01" & Ig_Date <= "2020-12-31") %>%
  mutate(burn_severity_max = case_when(!(High_T %in% c(-9999, 9999, -999, 999)) ~ "High",
                                       !(Mod_T %in% c(-9999, 9999, -999, 999)) ~ "Moderate",
                                       !(Low_T %in% c(-9999, 9999, -999, 999)) ~ "Low",
                                       !(IncGreen_T %in% c(-9999, 9999, -999, 999)) ~ "Increased Greeness / Unchanged"))

# Globfire dataset of all fires that are able to link to smoke 
globfire_daily_raw <- readRDS("raw_data/globfire_link_dataset_Oct10_2023/daily_globfire_link.rds")
# Polygon shapefile for the fires that are documented in the Globfire dataset
globfire_poly_raw <- st_read("raw_data/globfire_link_dataset_Oct10_2023/globfire_na_final_area_2006-2020.shp")
# Transform the globfire polygon shapefile so that it has the same coordinate reference system as MTBS
globfire_poly <- st_transform(globfire_poly_raw, crs = st_crs(mtbs_bab))

# Join MTBS and GlobFire ---------------------------------------------------
sf_use_s2(FALSE)
# Return MTBS features augmented with the fields of GlobFire that overlap with features of MTBS
fire_dat <- st_join(mtbs_bab, globfire_poly,
                    join = st_intersects) 
# Calculate overlapping area
intersection <- st_intersection(mtbs_bab, globfire_poly) %>% 
  mutate(intersect_area = st_area(.)) %>%
  dplyr::select(Event_ID, Id, intersect_area) %>%
  st_drop_geometry()
fire_dat <- fire_dat %>%
  mutate(polygon_area = st_area(fire_dat),
         diff_time = abs(difftime(Ig_Date, IDate, units = "days"))) %>%
  # Spatially matched fires must occur within 30 days of each other 
  filter(diff_time <= 30) %>%
  left_join(intersection, by = c("Event_ID", "Id")) %>%
  # Calculate overlapping area / MTBS area
  mutate(intersect_pct = intersect_area*100 / polygon_area)

# For non-unique matches (MTBS fire matched with multiple GlobFire), keep the match with largest overlapping area
fire_dat_uniq <- fire_dat %>%
  group_by(Event_ID) %>%
  filter(intersect_pct == max(intersect_pct)) %>%
  ungroup()

#summary(fire_dat_uniq$intersect_pct) 
# Keep matches with overlapping percentage > 1st Qu. 
fire_dat_uniq <- fire_dat_uniq %>%
  filter(as.numeric(intersect_pct) > as.numeric(quantile(fire_dat_uniq$intersect_pct)[2]))
saveRDS(fire_dat_uniq, file = "processed_data_all/fire_dat.RDS")
fire_dat <- readRDS(file = "processed_data_all/fire_dat.RDS")


# Burn severity ----------------------------------------------------------
# Function to get burn severity classes in the boundaries for each fire in a specific state and year
get_value_inboundaries <- function(location, fire_year) {
  mtbs_rast <- terra::rast(file.path("raw_data",
                                     paste0("MTBS_BSmosaics_", location),
                                     paste0(fire_year),
                                     paste0("mtbs_", location, "_", fire_year),
                                     paste0("mtbs_", location, "_", fire_year, ".tif")))
  mtbs_rast <- terra::project(mtbs_rast, llprj, method = "near")
  d <- subset(fire_dat, format(fire_dat$Ig_Date, "%Y") == fire_year & state == location)
  value_inboundaries <- terra::extract(mtbs_rast, vect(d), na.rm = TRUE)
  return(value_inboundaries)
}

# Function to get the mode for a vector 
get_mode <- function(v) {
  valid_values <- na.omit(v)
  uniqv <- unique(valid_values)
  uniqv[which.max(tabulate(match(valid_values, uniqv)))]
}

# Get the most common burn severity class in the boundaries for each fire
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
    saveRDS(d, file = paste0("processed_data_all/burn_severity_dat/", location, fire_year, ".RDS"))
  }
}
burn_severity_dat <- list.files( path = "processed_data_all/burn_severity_dat/", pattern = "*.RDS", full.names = TRUE ) %>%
  map_dfr(readRDS)
saveRDS(burn_severity_dat, file = "processed_data_all/burn_severity_dat.RDS")
unlink("processed_data_all/burn_severity_dat", recursive = TRUE)
#prop.table(table(burn_severity_dat$burn_severity_mode)) 

# Topography data --------------------------------------------------------
# Get state bounds
obj <- st_read("raw_data/cb_2020_us_state_500k/cb_2020_us_state_500k.shp")
CA_bound <- subset(obj, NAME == "California") %>% st_transform(crs = 4326)
FL_bound <- subset(obj, NAME == "Florida") %>% st_transform(crs = 4326)
GA_bound <- subset(obj, NAME == "Georgia") %>% st_transform(crs = 4326)

# Elevation
elev <- raster("raw_data/topography/elevation_1KMmd_GMTEDmd.tif")
elev_grid_ca_poly <- raster::extract(crop(elev, extent(CA_bound)), fire_dat %>% filter(state == "CA"), 
                                     fun = mean, na.rm = TRUE)
elev_grid_fl_poly <- raster::extract(crop(elev, extent(FL_bound)), fire_dat %>% filter(state == "FL"), 
                                     fun = mean, na.rm = TRUE)
elev_grid_ga_poly <- raster::extract(crop(elev, extent(GA_bound)), fire_dat %>% filter(state == "GA"), 
                                     fun = mean, na.rm = TRUE)

# Slope
slope <- raster("raw_data/topography/slope_1KMmd_GMTEDmd.tif")
slope_grid_ca_poly <- raster::extract(crop(slope, extent(CA_bound)), fire_dat %>% filter(state == "CA"), 
                                      fun = mean, na.rm = TRUE)
slope_grid_fl_poly <- raster::extract(crop(slope, extent(FL_bound)), fire_dat %>% filter(state == "FL"), 
                                      fun = mean, na.rm = TRUE)
slope_grid_ga_poly <- raster::extract(crop(slope, extent(GA_bound)), fire_dat %>% filter(state == "GA"), 
                                      fun = mean, na.rm = TRUE)

# Aspect sine
aspect_sin <- raster("raw_data/topography/aspectsine_1KMmd_GMTEDmd.tif")
aspectsin_grid_ca_poly <- raster::extract(crop(aspect_sin, extent(CA_bound)), fire_dat %>% filter(state == "CA"), 
                                          fun = mean, na.rm = TRUE)
aspectsin_grid_fl_poly <- raster::extract(crop(aspect_sin, extent(FL_bound)), fire_dat %>% filter(state == "FL"), 
                                          fun = mean, na.rm = TRUE)
aspectsin_grid_ga_poly <- raster::extract(crop(aspect_sin, extent(GA_bound)), fire_dat %>% filter(state == "GA"), 
                                          fun = mean, na.rm = TRUE)

# Aspect cosine
aspect_cos <- raster("raw_data/topography/aspectcosine_1KMmd_GMTEDmd.tif")
aspectcos_grid_ca_poly <- raster::extract(crop(aspect_cos, extent(CA_bound)), fire_dat %>% filter(state == "CA"), 
                                          fun = mean, na.rm = TRUE)
aspectcos_grid_fl_poly <- raster::extract(crop(aspect_cos, extent(FL_bound)), fire_dat %>% filter(state == "FL"), 
                                          fun = mean, na.rm = TRUE)
aspectcos_grid_ga_poly <- raster::extract(crop(aspect_cos, extent(GA_bound)), fire_dat %>% filter(state == "GA"), 
                                          fun = mean, na.rm = TRUE)

# Combine topography data
elev_grid_poly <- rbind(elev_grid_ca_poly, elev_grid_fl_poly, elev_grid_ga_poly) 
slope_grid_poly <- rbind(slope_grid_ca_poly, slope_grid_fl_poly, slope_grid_ga_poly)
aspectsin_grid_poly <- rbind(aspectsin_grid_ca_poly, aspectsin_grid_fl_poly, aspectsin_grid_ga_poly)
aspectcos_grid_poly <- rbind(aspectcos_grid_ca_poly, aspectcos_grid_fl_poly, aspectcos_grid_ga_poly)
topography_grid_poly <- cbind(elev_grid_poly, slope_grid_poly, aspectsin_grid_poly, aspectcos_grid_poly)
colnames(topography_grid_poly) <- c("elevation", "slope", "aspect_sin", "aspect_cos")
topography_dat <- rbind(fire_dat %>% filter(state == "CA"), fire_dat %>% filter(state == "FL"), fire_dat %>% filter(state == "GA")) %>%
  dplyr::select(Event_ID) %>%
  cbind(topography_grid_poly)
saveRDS(topography_dat, file = "processed_data_all/topography_dat.RDS")

# Landcover data ----------------------------------------------------------
# Data released in these years: 2006, 2008, 2011, 2013, 2016, 2019 
# Use data from the most recent release year 
fire_dat <- fire_dat %>%
  mutate(landcover_yr = case_when(Ig_Date >= "2006-01-01" & Ig_Date <= "2007-12-31" ~ "2006",
                                  Ig_Date >= "2008-01-01" & Ig_Date <= "2010-12-31" ~ "2008",
                                  Ig_Date >= "2011-01-01" & Ig_Date <= "2012-12-31" ~ "2011",
                                  Ig_Date >= "2013-01-01" & Ig_Date <= "2015-12-31" ~ "2013",
                                  Ig_Date >= "2016-01-01" & Ig_Date <= "2018-12-31" ~ "2016",
                                  Ig_Date >= "2019-01-01" & Ig_Date <= "2020-12-31" ~ "2019"))
for (year in as.character(c(2006, 2008, 2011, 2013, 2016, 2019))) {
  land_rast <- terra::rast(file.path("raw_data/NLCD_landcover_2021_release_all_files_20230630",
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
  saveRDS(d, file = paste0("processed_data_all/landcover_dat/", paste0("landcover_dat", year), ".RDS"))
}
landcover_dat <- list.files( path = "processed_data_all/landcover_dat/", pattern = "*.RDS", full.names = TRUE ) %>%
  map_dfr(readRDS)
saveRDS(landcover_dat, file = "processed_data_all/landcover_dat.RDS")
unlink("processed_data_all/landcover_dat", recursive = TRUE)

# Climate data (pre-fire) ----------------------------------------------------------
# Climate vars: precipitation, wind direc, wind veloc, pressure, min temp, max temp, min rel humidity, max rel humidity
climate_vars <- c("pr", "th", "vs", "vpd", "tmmn", "tmmx", "rmin", "rmax") 
climate_dat <- fire_dat 
sf_use_s2(FALSE)
for (v in 1:length(climate_vars)) {
  climate_dat_CA <- climate_dat %>% filter(state == "CA")
  layer_climate_fire <- NULL
  for (i in 1:nrow(climate_dat_CA)) {
    yr <- year(st_drop_geometry(climate_dat_CA)[i,]$Ig_Date)
    day_in_yr <- yday(st_drop_geometry(climate_dat_CA)[i,]$Ig_Date)
    stack <- stack(paste0("raw_data/climate/", climate_vars[v], "_", as.character(yr), ".nc"))
    stack_layer <- crop(stack[[day_in_yr - 1]], extent(CA_bound)) # take climate value from the day before Ig_Date
    layer_climate <- st_as_sf( rasterToPolygons(stack_layer) )
    colnames(layer_climate)[1] <- climate_vars[v]
    layer_climate_fire <- rbind(layer_climate_fire, st_join(climate_dat_CA[i,], layer_climate, join = st_intersects, largest = TRUE))
  }
  climate_dat_CA <- layer_climate_fire
   
  climate_dat_FL <- climate_dat %>% filter(state == "FL")
  layer_climate_fire <- NULL
  for (i in 1:nrow(climate_dat_FL)) {
    yr <- year(st_drop_geometry(climate_dat_FL)[i,]$Ig_Date)
    day_in_yr <- yday(st_drop_geometry(climate_dat_FL)[i,]$Ig_Date)
    stack <- stack(paste0("raw_data/climate/", climate_vars[v], "_", as.character(yr), ".nc"))
    stack_layer <- crop(stack[[day_in_yr - 1]], extent(FL_bound))
    layer_climate <- st_as_sf( rasterToPolygons(stack_layer) )
    colnames(layer_climate)[1] <- climate_vars[v]
    layer_climate_fire <- rbind(layer_climate_fire, st_join(climate_dat_FL[i,], layer_climate, join = st_intersects, largest = TRUE))
  }
  climate_dat_FL <- layer_climate_fire
  
  climate_dat_GA <- climate_dat %>% filter(state == "GA")
  layer_climate_fire <- NULL
  for (i in 1:nrow(climate_dat_GA)) {
    yr <- year(st_drop_geometry(climate_dat_GA)[i,]$Ig_Date)
    day_in_yr <- yday(st_drop_geometry(climate_dat_GA)[i,]$Ig_Date)
    stack <- stack(paste0("raw_data/climate/", climate_vars[v], "_", as.character(yr), ".nc"))
    stack_layer <- crop(stack[[day_in_yr - 1]], extent(GA_bound))
    layer_climate <- st_as_sf( rasterToPolygons(stack_layer) )
    colnames(layer_climate)[1] <- climate_vars[v]
    layer_climate_fire <- rbind(layer_climate_fire, st_join(climate_dat_GA[i,], layer_climate, join = st_intersects, largest = TRUE))
  }
  climate_dat_GA <- layer_climate_fire
  
  climate_dat <- rbind(climate_dat_CA, climate_dat_FL, climate_dat_GA)
}
climate_dat <- climate_dat[,c("Event_ID", climate_vars)]
saveRDS(climate_dat, file = "processed_data_all/climate_dat.RDS")

# Final data for causal analysis -----------------------------------------
fire_dat <- readRDS("processed_data_all/fire_dat.RDS") %>% st_drop_geometry()
burn_severity_dat <- readRDS("processed_data_all/burn_severity_dat.RDS") %>% st_drop_geometry()
topography_dat <- readRDS("processed_data_all/topography_dat.RDS") %>% st_drop_geometry()
landcover_dat <- readRDS("processed_data_all/landcover_dat.RDS") %>% st_drop_geometry()
climate_dat <- readRDS("processed_data_all/climate_dat.RDS") %>% st_drop_geometry()
smoke_dat <- readRDS("processed_data_all/sorted_total_smokepm_dt.RDS")

dat <- fire_dat %>% 
  dplyr::select(c("Event_ID", "Incid_Type", "Ig_Date", "state", "Id", "IDate", "FDate", "polygon_area")) %>%
  rename(fire_type = Incid_Type,
         ig_date = Ig_Date,
         globfire_id = Id) %>%
  mutate(globfire_id = as.character(globfire_id))
dat <- purrr::reduce(list(dat, burn_severity_dat, topography_dat, landcover_dat, climate_dat), dplyr::left_join, by = "Event_ID") %>%
  rename(mtbs_id = Event_ID) %>%
  left_join(smoke_dat %>% dplyr::select(c(total_pop_smokePM, total_smokePM, fire_id)), 
            by = join_by(globfire_id == fire_id)) 
sum(!is.na(dat$total_pop_smokePM)) # 1369/2191 obs have smoke outcome 
saveRDS(dat, file = "processed_data_all/dat.RDS")
