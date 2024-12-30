library(dplyr)
library(sf)
# Run this script from rezoner subdirectory
setwd('~/Desktop/rezoner2/rezoner')
df <- readRDS('../five_rezonings_processed.RDS')
tax <- st_read('../Assessor Historical Secured Property Tax Rolls_20240121.geojson')

parcels_to_exclude <- c('State of California Property', 'Under Water Lot')
# Get bad parcels
bad_parcels <- tax %>%
  filter((property_class_code_definition %in% parcels_to_exclude) | 
           (exemption_code_definition == 'Cemetary') | (block == '9900') |
           (block == '0006' & lot == '001') | (block == '1300' & lot == '001') |
           (block == '0409' & lot == '002') | (block == '7501'))
           
           #(block == '1313' & lot == '016') | (block == '1461' & lot == '001') |
           #(block == '1849' & lot == '054') | (block == '1481') |
           #(block == '1756' & lot == '001') | (block == '7501'))
to_exclude <- st_union(bad_parcels)
filtered <- st_filter(df, to_exclude, .predicate=st_disjoint)
nrow(filtered)

filtered <- filtered[filtered$ex_height2024 < 1111,]

filtered <- filtered %>% select(-MapBlkLot_Master, -Developed,
              -M4_height, -M5_height, -EX_USE,
              -M1_CAP, -M2_CAP, -M3_CAP,
              -M1_GP_TYPE, -M2_GP_TYPE, -M3_GP_TYPE, -VACANT,
              -sup_dist_name, -EX_GP_TYPE, -EX_ZONING)


# Remove pipeline
pipeline <- st_read('../data/SF Development Pipeline 2022 Q1 [REVISED]_20240331.geojson')
pipeline <- pipeline[!is.na(pipeline$pipeline_units) 
                      & (as.numeric(pipeline$pipeline_units) > 0)
                      & !(is.na(pipeline$unitsnet)) 
                      & (as.numeric(pipeline$unitsnet) > 0)
                      & !(st_is_empty(pipeline)),]
pipeline_points <- st_union(pipeline)
filtered <- st_filter(filtered, pipeline_points, .predicate=st_disjoint)
nrow(filtered)

fourplex <- 'Increased density up to four units'
sixplex <- 'Increased density up to six units'
filtered[!is.na(filtered$M1_ZONING) & (filtered$M1_ZONING %in% c(fourplex, sixplex)) & filtered$ex_height2024 > 65, 'M1_ZONING'] <- NA
filtered[!is.na(filtered$M2_ZONING) & (filtered$M2_ZONING %in% c(fourplex, sixplex)) & filtered$ex_height2024 > 65, 'M2_ZONING'] <- NA
filtered[!is.na(filtered$M3_ZONING) & (filtered$M3_ZONING %in% c(fourplex, sixplex)) & filtered$ex_height2024 > 65, 'M3_ZONING'] <- NA
filtered[!is.na(filtered$M4_ZONING) & (filtered$M4_ZONING %in% c(fourplex, sixplex)) & filtered$ex_height2024 > 65, 'M4_ZONING'] <- NA
filtered[!is.na(filtered$M5_ZONING) & (filtered$M5_ZONING %in% c(fourplex, sixplex)) & filtered$ex_height2024 > 65, 'M5_ZONING'] <- NA


# Remove lots under highways
streets <- st_read('../data/Streets   Active and Retired_20240331.geojson')
highways <- streets[streets$layer == 'FREEWAYS' & streets$active == 'true',]
highway_lines <- st_union(highways)
filtered <- st_filter(filtered, highway_lines, .predicate=st_disjoint)
nrow(filtered)

saveRDS(filtered, '../five_rezonings_filtered.RDS')
