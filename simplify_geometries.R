## Take geo df with 4 rezonings
## Output one geo df with primary_group_key and geometry
## Output second geo df which is like first geodf but with primary_group_key

library(shiny)
library(dplyr)
library(sf)
library(leaflet)
library(waiter)
library(shinyjs)
library(sfarrow)

# When I try grouping by M5, parks start getting included. Need to filter out parcels first before simplify
setwd('~/Desktop/rezoner2/')
df <- st_read_feather('./rezoner/four_rezonings_v4.feather')
block_zones <- df %>% 
  mutate(block = stringr::str_sub(mapblklot, 1, 4)) %>%
  dplyr::group_by(M1_ZONING, M2_ZONING, M3_ZONING, M4_ZONING, block) %>%
  summarise(.groups='keep') %>%
  ungroup()
#block_zones <- st_union(block_zones, by_feature=T)
block_zones <- st_cast(block_zones, "MULTIPOLYGON")
#block_zones <- st_make_valid(block_zones)
#block_zones <- st_make_valid(block_zones)
block_zones <- st_make_valid(block_zones, 'MULTIPOLYGON')
print(sum(st_area(block_zones)))
block_simple <- rmapshaper::ms_simplify(block_zones, keep=.1) #, keep_shapes=T
block_simple <- st_make_valid(block_simple, 'MULTIPOLYGON')

#block_simple <- st_simplify(block_zones, dTolerance=10)
print(sum(st_area(block_simple)))
block_simple['primary_key'] = 1:nrow(block_simple)
block_simple <- st_sf(block_simple)
no_geo_bs <- st_drop_geometry(block_simple)
block_simple <- select(block_simple, M1_ZONING, M2_ZONING, M3_ZONING, 
                       M4_ZONING, block, primary_key, geometry)
block_simple <- st_cast(block_simple, "MULTIPOLYGON")
st_write_feather(block_simple, './rezoner/simple_geometries.feather')

no_geo_bs <- st_drop_geometry(block_simple)
result <- left_join(df, no_geo_bs,
                    by=c('M1_ZONING', 'M2_ZONING', 
                         'M3_ZONING', 'M4_ZONING',
                         'block'))
result <- select(result, -block)
st_write_feather(result, './four_rezonings_v2.feather')
getwd()
