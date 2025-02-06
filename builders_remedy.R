setwd('~/Desktop/rezoner2/rezoner')
library(sf)
library(dplyr)

### NOTE ###
# This script has one job: take the geodataframe from 
# five_rezonings_processed.RDS 
# and append a column named BR_ZONING with heights after builder's remedy
# and save the new augmented dataframe to five_rezonings_processed_br.RDS
# This will then affect the web app if you run these three scripts in sequence
# 1) filter_out_extraneous.R 2) to_mapbox.R 3) app.R
# It's ok for this script to have side-effects for df as long as it doesnt overwrite
# values for columns in this df '../five_rezonings_processed.RDS'
# It's also not the end of the world if the height saved for BR_ZONING is less
# the existing height already allowed on the parcel; these parcels don't factor
# into the added_capacity estimated in app.R

df <- readRDS('../five_rezonings_processed.RDS')
mullin_density <- 30
df['mullin'] <- 1.5 * mullin_density
df['housing_element'] <- pmax(df$M1_MAXDENS, df$M2_MAXDENS, df$M3_MAXDENS, na.rm=T)


# I used conservative version of sb 9 that only allows 3 units
# TODO: add a toggle to make less conservative (low priority)
# I am assuming sb 9 applies only where local 4plex zoning applies
sf_use_s2(FALSE)
df['sb9'] = 0
fourplex <- st_read('../fourplex_areas_12_2023.geojson')
fourplex <- st_transform(fourplex, st_crs(df))
intersections <- st_join(df, fourplex, left=F)
df$sb9[
  (df$mapblklot %in% intersections$mapblklot)
] <- 3
df$sb9 <- df$sb9 / df$ACRES # to get du / acre
sf_use_s2(TRUE)


# TODO: high priority - fix sb 2011
df['sb2011'] <- 0.0
df['state_law'] = 3 * pmax(df$sb9, df$sb2011, na.rm=T)

# TODO: 2x check there's no public, geospatial dataset of du/acre for density-restricted zoning designations
df['local_zoning'] <- 0
# I can get du/acre for form based areas by using the city's formula from the housing element appendix for E[U|D]
# and divide by acreage.
df$local_zoning[as.logical(df$zp_FormBasedMulti)] <- df$expected_units_baseline_if_dev[as.logical(df$zp_FormBasedMulti)] / df$ACRES[as.logical(df$zp_FormBasedMulti)]
df['local_zoning'] <- df$local_zoning * 3

df['builders_remedy_du_acre'] <- pmax(df$local_zoning, df$state_law, df$mullin, df$housing_element, na.rm=T)


# TODO: Only add increment where tcac = 'high' or 'highest' OR one-half mile of a major transit stop OR 
# "A very low vehicle travel area
# Blocker for this is getting a geojson defining one half mile of a major transit stop and low vehicle travel area
# from this website: https://sitecheck.opr.ca.gov/
# which only has arcgis format (which is lame)
increment <- 35
df['builders_remedy_du_acre'] <- df$builders_remedy_du_acre + increment


max_envelope <- max(df$Envelope_1000)

calculate_du_count <- function(height, ACRES, building_efficiency_discount) {
  # returns # of homes a potential project can contain at a certain height
  height_deduction <- ifelse(height <= 50, 10, 15)
  n_floors_residential <- (height - height_deduction) %/% 10
  lot_coverage_discount <- ifelse(ACRES > 1, 0.55, 0.75)
  ground_floor <- (ACRES * 43560) * lot_coverage_discount
  if ((ACRES * 43560) <= 12000 && height > 85) {
    n_floors_residential <- pmin(n_floors_residential, 12)
  }
  lot_sqft <- ACRES * 43560
  print(ground_floor)
  print(n_floors_residential)
  if (height <= 85) {
    expected_built_envelope <- ground_floor * n_floors_residential
  } else if (height > 85 && lot_sqft < 12000) {
    expected_built_envelope <- ground_floor * n_floors_residential
  } else if (height > 85 && lot_sqft < 45000) {
    expected_built_envelope <- ground_floor * 7 + 12000 * pmax(n_floors_residential - 7, 0)
  } else {
    expected_built_envelope <- ground_floor * 7 + round(ACRES) * 12000 * pmax(n_floors_residential - 7, 0)
  }
  expected_built_envelope <- expected_built_envelope * building_efficiency_discount
  expected_built_envelope <- pmin(expected_built_envelope, max_envelope * 1000)
  du_count <- expected_built_envelope / 850

  return(du_count)
}

find_required_height <- function(ACRES, dwellings_per_acre, building_efficiency_discount = 0.8, height_search_max = 1000) {
  # BR gives us a du/acre, but my app needs a height to predict capacity
  # the city gave us a methodology for going from height to du/acre, and
  # so im reverse engineering it to see how tall the building must be to allow the du/acre
  # allowed by BR
  
  required_units <- ACRES * dwellings_per_acre
  last_du_count = 0
  for (test_height in seq(40, height_search_max, by = 10)) {
    du_count <- calculate_du_count(
      height = test_height,
      ACRES = ACRES,
      building_efficiency_discount = building_efficiency_discount
    )
    print(du_count)
    if (du_count >= required_units) {
      return(test_height)
    }
    if (last_du_count == du_count & test_height > 85 && ACRES < (45000 /43560) ) {
      print('edge case')
      # edge case for small ltos with towers, where they cant add more density
      return(test_height)
    }
    last_du_count = du_count
  }

  return(NA)
}

df <- df %>%
  mutate(BR_ZONING = mapply(find_required_height, ACRES, builders_remedy_du_acre))


saveRDS(df, '../five_rezonings_processed_br.RDS')
