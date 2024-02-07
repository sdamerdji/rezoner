library(sf)
library(stringr)
library(sfarrow)
library(dplyr)
sf_use_s2(FALSE)
start <- Sys.time()

# This script takes an hour to run.
# It may take less time if sf_use_s2(T) but then there are invalid geometries
# Maybe crs changes can avoid sf_use_s2(F)?

setwd('~/Desktop/rezoner2/rezoner')
df <- st_read_feather('../four_rezonings_v2.feather')

###### Heights ######
heights <- st_read('../Zoning Map - Height and Bulk Districts_20240121.geojson')
heights <- select(heights, -height) %>%
  rename(ex_height2024 = gen_hght) %>%
  mutate(ex_height2024 = as.numeric(ex_height2024))
results <- st_join(df, heights, largest=T)

##### High Opportunity Overlay #####
equity <- st_read('../final_opp_2024_public.gpkg')
equity_sf <- st_filter(equity, st_union(df))
high_opp <- st_union(equity_sf[equity_sf$oppcat %in% c('High Resource', 'Highest Resource'),])
saveRDS(st_union(high_opp), './high_opp.RDS')

##### AFFH Score + Economic Opportunity Score #####
econ_opp <- st_read('../final_2023_shapefile/final_2023_public.shp')
econ_opp <- econ_opp %>% 
  st_filter(st_union(df)) %>%
  rename(econ_affh = ecn_dmn, affh2023=oppcat) %>%
  select(econ_affh, affh2023)
results <- st_join(results, econ_opp, largest=T)


###### Priority equity geographies ###### 
sud <- st_read('../Zoning Map - Special Use Districts_20240122.geojson')
peg <- sud[sud$name == 'Priority Equity Geographies Special Use District',]$geometry
saveRDS(peg, './peg.RDS')
results[, 'peg'] <- as.vector(st_intersects(results, peg, sparse=F))

print(Sys.time() - start)

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
model <- readRDS(file='./light_model.rds')
predictions.16 <- predict(model, newdata = results, type = "response")
# E[U | Baseline]
results <- results %>%
  mutate(pdev_baseline_1yr = predictions.16) %>% 
  mutate(expected_units_baseline_if_dev = Envelope_1000 * 1000 * 0.8 / 850)

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
         Envelope_1000_new = case_when(
           height >= 85 ~ ((ACRES * 43560) * 0.8 * height / 10.5) / 1000,
           ACRES >= 1 & height < 85 ~ ((ACRES * 43560) * 0.55 * height / 10.5) / 1000,
           ACRES < 1 ~ ((ACRES * 43560) * 0.75 * height / 10.5) / 1000, # Swap lines from Rmd bc this logic matches STATA code
           TRUE ~ NA_real_
         ),
         # no downzoning allowed
         existing_sqft = if_else(Upzone_Ratio != 0, Envelope_1000 / Upzone_Ratio, 0),
         Envelope_1000 = pmax(Envelope_1000_new, Envelope_1000),
         Envelope_1000 = pmin(Envelope_1000, max_envelope),
         Upzone_Ratio = if_else(existing_sqft > 0, Envelope_1000 / existing_sqft, 0), # This, to me, is wrong, but it's how Blue Sky data is coded
         expected_units_skyscraper_if_dev = Envelope_1000 * 1000 * 0.8 / 850) # Should 0.8 this be here?

predictions.16_skyscraper <- predict(model, newdata = temp_df, type = "response")
results[,'expected_units_skyscraper_if_dev'] <- temp_df$expected_units_skyscraper_if_dev
results[,'pdev_skyscraper_1yr'] <- predictions.16_skyscraper

print(paste('Potential', round(Sys.time() - start, 1)))

##### Supervisor Breakdown #####
supervisors <- st_read('../Supervisor Districts (2022)_20240124.geojson')

nrow(results)
results2 <- st_join(results, supervisors, largest=T)
nrow(results2)
results2 <- select(results2, -c("sup_dist_pad", "sup_dist_num", 
              "data_loaded_at", "sup_dist", "data_as_of"))
points <- st_coordinates(st_centroid(results2))
results2[, 'lng'] <- points[,1]
results2[, 'lat'] <- points[,2]
st_write_feather(results2, '../four_rezonings_v3.feather')


###### MUNI LINES ######
setwd('~/Desktop/rezoner2/rezoner')
df <- st_read_feather('../four_rezonings_v3.feather')
muni <- st_read('../Muni Simple Routes_20240202.geojson')
frequent_lines <- muni[(muni$service_ca == '10 Minutes or Less'),]
nearest = st_nearest_feature(df, frequent_lines)
dist = st_distance(df, frequent_lines[nearest,], by_element=TRUE)
df['transit_dist'] <- as.numeric(dist / 1609)

# Now just rapid muni lines
rapid_lines <- muni[str_detect(muni$lineabbr, 'R$'),]
nearest = st_nearest_feature(df, rapid_lines)
dist = st_distance(df, rapid_lines[nearest,], by_element=TRUE)
df['transit_dist_rapid'] <- as.numeric(dist / 1609)


###### BART & CALTRIAIN #######
# Bart
setwd('~/Desktop/rezoner2/rezoner')
bart <- st_read('../BART_System_2020/BART_Station.geojson')
nearby_stops <- bart[bart$City %in% c('San Francisco', 'Daly City'),] # Reduce compute time
nearest = st_nearest_feature(df, nearby_stops)
dist = st_distance(df, nearby_stops[nearest,], by_element=TRUE)
df['transit_dist_bart'] <- as.numeric(dist / 1609)

# Caltrain
caltrain <- st_read('../Caltrain Stations and Stops.geojson')
nearest = st_nearest_feature(df, caltrain)
dist = st_distance(df, caltrain[nearest,], by_element=TRUE)
df['transit_dist_caltrain'] <- as.numeric(dist / 1609)

print(paste('Transit', round(Sys.time() - start, 1)))

###### COMMERCIAL CORRIDORS ###### 
zoning <- st_read('../Zoning Map - Zoning Districts.geojson')
commercial_corridors <- zoning[str_detect(zoning$zoning, '^(NCT)|(RCD)|(NC)|(MU)'),] #TODO: Ask Annie if correct

nearest = st_nearest_feature(df, commercial_corridors)
dist = st_distance(df, commercial_corridors[nearest,], by_element=TRUE)
df['commercial_dist'] <- as.numeric(dist / 1609)

####### NEIGHBORHOODS ####### 
hoods <- st_read('../Analysis Neighborhoods_20240202.geojson')
saveRDS(sort(hoods$nhood), './hoods.RDS')
result <- st_join(df, hoods, largest=T)


#### TRY RDS INSTEAD OF FEATHER. IT TAKES A THIRD OF THE TIME. ####
saveRDS(result, './four_rezonings_v4.RDS')
print(paste('All', Sys.time() - start))

