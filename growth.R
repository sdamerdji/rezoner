library(sf)
library(stringr)
sf_use_s2(FALSE)


#Start with every parcel
#1.	Rm parcels already rezoned for >= 85’ in this map
#2.	Rm PEG
#3.	Rm existing multifamily use or zoned multifamily
#4.	Rm if less than 2500 sq ft



df <- st_read_feather('./four_rezonings_v2.feather')


# Get low opportunity areas in SF
equity <- st_read('../final_opp_2024_public.gpkg')
equity_sf <- st_filter(equity, st_union(df))
low_opp <- st_union(equity_sf[equity_sf$oppcat == 'Low Resource', 'oppcat'])

# Heights
heights <- st_read('../Zoning Map - Height and Bulk Districts_20240121.geojson')
heights <- select(heights, -height) %>%
  rename(ex_height2024 = gen_hght) %>%
  mutate(ex_height2024 = as.numeric(ex_height2024))


results <- st_join(df, heights, largest=T)

high_opp <- st_union(equity_sf[equity_sf$oppcat %in% c('High Resource', 'Highest Resource'),])
saveRDS(st_union(high_opp), './high_opp.RDS')

results[, 'high_opportunity'] <- st_intersects(results, high_opp, sparse=F)

# Add econ opp score
econ_opp <- st_read('../final_2023_shapefile/final_2023_public.shp')
econ_opp <- econ_opp %>% 
  st_filter(st_union(df)) %>%
  rename(econ_affh = ecn_dmn, affh2023=oppcat) %>%
  select(econ_affh, affh2023)

results <- st_join(results, econ_opp, largest=T)

# Existing Use
tax <- st_read('../Assessor Historical Secured Property Tax Rolls_20240121.geojson')
mfr <- tax[tax$use_definition == 'Multi-Family Residential',]
results_no_mfr <- st_filter(results, st_union(mfr), .predicate = st_disjoint)

acres_to_sqft <- 43560
height_max <- 85
df2 <- results_no_mfr %>%
  st_filter(low_opp, .predicate=st_disjoint) %>%
  filter(ACRES * acres_to_sqft >= 2500) %>%
  filter(!zp_DensRestMulti & !zp_FormBasedMulti) %>%
  filter(is.na(M1_ZONING) | (as.numeric(str_extract(M1_ZONING, "\\d+")) < height_max)) %>%
  filter(is.na(M2_ZONING) | (as.numeric(str_extract(M2_ZONING, "\\d+")) < height_max)) %>%
  filter(is.na(M3_ZONING) | (as.numeric(str_extract(M3_ZONING, "\\d+")) < height_max)) %>%
  filter(is.na(M4_ZONING) | (as.numeric(str_extract(M4_ZONING, "\\d+")) < height_max)) %>%
  filter(is.na(ex_height2024) | (ex_height2024 < height_max))


plot(df2[,'ex_height2024'])
saveRDS(st_union(df2), './growth.RDS')
plot(low_opp)

st_write_feather(results, './four_rezonings_v3.feather')
