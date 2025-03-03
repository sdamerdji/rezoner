library(sf)
library(stringr)
library(dplyr)
library(tidytransit)
library(parallel)
library(foreach)
library(doParallel)

building_efficiency_discount <- .8
sdbl <- 1.24
typical_unit_size <- 850
sf_use_s2(F)

centroid_join <- function(parcels_df, auxiliary_df) {
  init_nrows <- nrow(parcels_df)
  centroids <- st_point_on_surface(parcels_df)
  centroids['id'] <-  1:nrow(centroids)
  auxiliary_df <- st_transform(auxiliary_df, st_crs(parcels_df))
  result <- st_join(centroids, auxiliary_df, join = st_intersects) %>%
    group_by(id) %>%
    slice(1) %>%
    ungroup()
  result$geometry <- parcels_df$geometry
  final_nrows <- nrow(result)
  assertthat::are_equal(init_nrows, final_nrows)
  return(dplyr::select(result, -id))
}

parallelize_nearest_dist <- function(parcels_df, auxiliary_df) {
  n_cores = detectCores() - 1
  chunks <- split(parcels_df, cut(seq_len(nrow(parcels_df)), n_cores, labels = FALSE))
  
  registerDoParallel(cores = n_cores)
  
  nearest_parallel <- function(df_chunk) {
    dists <- sapply(1:nrow(df_chunk), function(i) {
      nearest = st_nearest_feature(df_chunk[i, ], auxiliary_df)
      dist = st_distance(df_chunk[i, ], auxiliary_df[nearest, ], by_element = TRUE, tolerance=0.01 * 1609)
      as.numeric(dist / 1609) # Convert meters to miles
    })
    return(dists)
  }
  
  result <- foreach(chunk = chunks, .combine = 'c') %dopar% {
    nearest_parallel(chunk)
  }
  return(result)
}

preprocess <- function() {
start <- Sys.time()
df <- readRDS(file.path(PROJECT_DIR, 'five_rezonings.RDS'))

###### Heights ######
heights <- st_read(file.path(DATA_DIR, 'Zoning Map - Height and Bulk Districts_20240121.geojson'))
heights <- select(heights, -height) %>%
  rename(ex_height2024 = gen_hght) %>%
  mutate(ex_height2024 = as.numeric(ex_height2024))
results <- centroid_join(df, heights)
rm('heights')

##### High Opportunity Overlay #####
equity <- st_read(file.path(DATA_DIR, 'final_opp_2024_public.gpkg'))
equity_sf <- st_filter(equity, st_union(results))
high_opp <- st_union(equity_sf[equity_sf$oppcat %in% c('High Resource', 'Highest Resource'),])
saveRDS(st_union(high_opp), file.path(APP_DIR, 'high_opp.RDS'))
rm('equity', 'equity_sf', 'high_opp')

##### AFFH Score + Economic Opportunity Score #####
econ_opp <- st_read(file.path(DATA_DIR, 'final_2023_shapefile/final_2023_public.shp'))
econ_opp <- econ_opp %>% 
  st_filter(st_union(df)) %>%
  rename(econ_affh = ecn_dmn, affh2023=oppcat) %>%
  select(econ_affh, affh2023)
results <- centroid_join(results, econ_opp)
results[is.na(results$affh2023), 'affh2023'] <- 'No data'
rm('econ_opp')

###### Priority equity geographies ###### 
sud <- st_read(file.path(DATA_DIR, 'Zoning Map - Special Use Districts_20240122.geojson'))
peg <- sud[sud$name == 'Priority Equity Geographies Special Use District',]$geometry
saveRDS(peg, file.path(APP_DIR, 'peg.RDS'))
results[, 'peg'] <- as.vector(st_intersects(results, peg, sparse=F))
rm('sud', 'peg')


results['ZONING'] <- NA

##### SB 330 Applies #####
results <- results %>%
  mutate(sb330_applies = case_when(
    # If all are false, then it's R1, so SB 330 applies
    (zp_OfficeComm + zp_DensRestMulti + zp_FormBasedMulti + zp_PDRInd + zp_Public + zp_Redev + zp_RH2 + zp_RH3_RM1) == 0 ~ 1,
    # If any of the other residential zones are 1, then SB 330 applies.
    zp_DensRestMulti == 1 | zp_FormBasedMulti == 1 | zp_RH3_RM1 == 1 | zp_RH2 == 1 | zp_Redev == 1 ~ 1,
    TRUE ~ 0
  ))


# Add a column for E[U | Baseline] and E[U | Skyscraper]
model <- readRDS(file.path(APP_DIR, 'light_model.rds'))
predictions.16 <- predict(model, newdata = results, type = "response")
# E[U | Baseline]
results <- results %>%
  mutate(pdev_baseline_1yr = predictions.16) %>% 
  mutate(expected_units_baseline_if_dev = if_else(Envelope_1000 * 1000 / typical_unit_size > 5,
                                                  Envelope_1000 * sdbl * 1000 / typical_unit_size,
                                                  Envelope_1000 * 1000 / typical_unit_size))

# E[U | Skyscraper]
skyscrapers <-  "245' Height Allowed"

max_envelope <- max(results$Envelope_1000, na.rm=T)

temp_df <- results %>%
  mutate(zp_OfficeComm = 0,
         zp_PDRInd = 0,
         zp_Public = 0,
         zp_Redev = 0,
         zp_RH2 = 0,
         zp_RH3_RM1 = 0,
         zp_FormBasedMulti = 1,
         zp_DensRestMulti = 0,
         height = 245,
         height_deduction = if_else(height <= 50, 10, 15),
         n_floors_residential = (height - height_deduction) %/% 10,
         
         # Lot coverage discount -> affects Envelope_1000
         lot_coverage_discount = if_else(ACRES > 1, .55, .75),
         
         # Envelope_1000
         ground_floor = (ACRES * 43560) * lot_coverage_discount,
         n_floors_residential = if_else((ACRES * 43560 <= 12000 & (height > 85)), # Cap at 12 for towers on small lots
                                        pmin(n_floors_residential, 12), 
                                        n_floors_residential),
         expected_built_envelope = case_when(
           height <= 85 ~ ground_floor * n_floors_residential,
           height > 85 & (ACRES * 43560 < 12000) ~ ground_floor * n_floors_residential,
           height > 85 & (ACRES * 43560 < 45000) ~ ground_floor * 7 + 12000 * pmax(n_floors_residential - 7, 0),
           TRUE ~ ground_floor * 7 + round(ACRES) * 12000 * pmax(n_floors_residential - 7, 0)
         ),
         expected_built_envelope = expected_built_envelope * building_efficiency_discount,
         expected_built_envelope = pmin(expected_built_envelope, max_envelope * 1000),
         Envelope_1000_new = expected_built_envelope / 1000,

         Envelope_1000_new = pmin(Envelope_1000_new, max_envelope), # Avoid outliers.
         expected_units_if_dev = expected_built_envelope / typical_unit_size,
         
         existing_sqft = if_else(Upzone_Ratio != 0, Envelope_1000 / Upzone_Ratio, 0),
         
         # No downzoning allowed
         Envelope_1000 = pmax(Envelope_1000_new, Envelope_1000),
         
         # Upzone ratio
         Upzone_Ratio = if_else(existing_sqft > 0, Envelope_1000 / existing_sqft, 0), # This, to me, is wrong, but it's how Blue Sky data is coded
         
         expected_units_skyscraper_if_dev = Envelope_1000 * 1000 / 850)


predictions.16_skyscraper <- predict(model, newdata = temp_df, type = "response")
results[,'expected_units_skyscraper_if_dev'] <- temp_df$expected_units_skyscraper_if_dev
results[,'pdev_skyscraper_1yr'] <- predictions.16_skyscraper

##### Supervisor #####
supervisors <- st_read(file.path(DATA_DIR, 'Supervisor Districts (2022)_20240124.geojson'))

nrow(results)
results <- centroid_join(results, supervisors)
results <- select(results, -c("sup_dist_pad", "sup_dist_num", 
              "data_loaded_at", "sup_dist", "data_as_of"))
rm('supervisors')
gc()
###### Lat, long for parcels #####
points <- st_coordinates(st_centroid(results))
results[, 'lng'] <- points[,1]
results[, 'lat'] <- points[,2]


###### Transit ######
gtfs_obj <- read_gtfs(file.path(DATA_DIR, 'google_transit.zip'))
muni_geo <- gtfs_as_sf(gtfs_obj, crs=st_crs(results))

# All Muni lines with <15 min frequency
uq_trips <- muni_geo$trips[!duplicated(muni_geo$trips[,c('route_id', 'shape_id')]),]
muni <- inner_join(uq_trips, muni_geo$shapes)
high_frequency_routes <- get_route_frequency(gtfs_obj) %>% arrange(median_headways) %>% filter(median_headways < 60 * 15) %>% select(route_id)
muni <- muni[muni$route_id %in% high_frequency_routes$route_id, ]
muni <- st_as_sf(muni, crs=st_crs(results))
results['transit_dist'] <- parallelize_nearest_dist(results[, 'geometry'], muni)

# Now just rapid muni lines
rapid_lines <- muni[str_detect(muni$route_id, 'R$') | muni$route_id %in% c('J', 'L', 'K', 'M', 'N', 'T'),]
results['transit_dist_rapid'] <- parallelize_nearest_dist(results[, 'geometry'], rapid_lines)

# Now just rapid muni stops
rapid_lines <- muni[str_detect(muni$route_id, 'R$') | muni$route_id %in% c('J', 'L', 'K', 'M', 'N', 'T'),]
# Find stops 
rapid_stops <- inner_join(rapid_lines, gtfs_obj$stop_times, by='trip_id')
rapid_stop_sf <- inner_join(st_drop_geometry(rapid_stops), gtfs_obj$stops)
rapid_stops <- st_as_sf(rapid_stop_sf, coords=c('stop_lon', 'stop_lat'), crs=st_crs(results))
results['transit_dist_rapid_stops'] <- parallelize_nearest_dist(results[, 'geometry'], rapid_stops)

###### BART & Caltrain #######
# Bart
bart <- st_read(file.path(DATA_DIR, 'BART_System_2020/BART_Station.geojson'))
nearby_bart_stops <- bart[bart$City %in% c('San Francisco', 'Daly City'),] # Reduce compute time
results['transit_dist_bart'] <- parallelize_nearest_dist(results[, 'geometry'], nearby_bart_stops)


# Caltrain
caltrain <- st_read(file.path(DATA_DIR, 'Caltrain Stations and Stops.geojson'))
results['transit_dist_caltrain'] <- parallelize_nearest_dist(results[, 'geometry'], caltrain)


###### Commercial Corridors ###### 
zoning <- st_read(file.path(DATA_DIR, 'Zoning Map - Zoning Districts.geojson'))
commercial_corridors <- zoning[str_detect(zoning$zoning, '^(NCT)|(RCD)|(NC)|(MU)'),]
results['commercial_dist'] <- parallelize_nearest_dist(results[, 'geometry'], commercial_corridors)
rm('commercial_corridors', 'zoning', 'rapid_stops', 'rapid_stop_sf', 'rapid_lines', 'bart', 'gtfs_obj')
gc()

####### Neighborhoods ####### 
hoods <- st_read(file.path(DATA_DIR, 'Analysis Neighborhoods_20240202.geojson'))
saveRDS(sort(hoods$nhood), file.path(APP_DIR, 'hoods.RDS'))
results <- centroid_join(results, hoods)


####### Parks ####### 
parks <- st_read(file.path(DATA_DIR, 'Park Lands - Recreation and Parks Department.geojson'))
parks$acres = as.numeric(parks$acres)
parks = parks[parks$map_park_n != 'Camp Mather',]
parks = parks[parks$acres > 1,]
results['park_dist'] <- parallelize_nearest_dist(results[, 'geometry'], parks)

###### Colleges ######
colleges <- st_read(file.path(DATA_DIR, 'Schools_College_20240215.geojson'))
colleges = colleges[as.numeric(st_area(colleges)) / 4047 > 1,]
results['college_dist'] <- parallelize_nearest_dist(results[, 'geometry'], colleges)


###### Save results to disk #####
saveRDS(results, file.path(PROJECT_DIR, 'five_rezonings_processed.RDS'))

print(Sys.time() - start)
}


if (sys.nframe() == 0) {
  preprocess()
}
