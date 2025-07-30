library(dplyr)
library(sf)

source('./constants.R')

parcels_to_exclude <- c('State of California Property', 'Under Water Lot')

filter_extraneous <- function() {
df <- readRDS(file.path(PROJECT_DIR, 'five_rezonings_processed_br.RDS'))
tax <- st_read(file.path(PROJECT_DIR, 'Assessor Historical Secured Property Tax Rolls_20240121.geojson'))

# Get bad parcels. 
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
nrow(filtered) # I lose 36 parcels doing this, but theyre all density decontroled.

filtered <- filtered[filtered$ex_height2024 < 1111,] # I lose 22 parcels doing this, but 13 are 40' and none exceed 85' of zoning

filtered <- filtered %>% select(-MapBlkLot_Master, -Developed,
              -M4_height, -M5_height, -EX_USE,
              -M1_CAP, -M2_CAP, -M3_CAP,
              -M1_GP_TYPE, -M2_GP_TYPE, -M3_GP_TYPE, -VACANT,
              -sup_dist_name, -EX_GP_TYPE, -EX_ZONING)


# Remove pipeline
# 120' Height Allowed 140' Height Allowed   160' Height Allowed   240' Height Allowed 
# 5                   12                    1                     7                   
# 350' Height Allowed  40' Height Allowed   50' Height Allowed    500' Height Allowed 
# 3                    575                  62                    1 
# 65' Height Allowed  650' Height Allowed   85' Base Height       85' Height Allowed 
# 81                   2                    4                     85 
# The most upzoned lots excluded by the pipeline are: 0574015, 0595006, 0595008, 0718020, 0691005, 0714016
pipeline <- st_read(file.path(DATA_DIR, 'SF Development Pipeline 2022 Q1 [REVISED]_20240331.geojson'))
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
filtered[!is.na(filtered$M6_ZONING_base) & (filtered$M6_ZONING_base %in% c(fourplex, sixplex)) & filtered$ex_height2024 > 65, 'M6_ZONING_base'] <- NA


# Remove lots under highways
# Includes: 3503002, 6755024, 6755025
streets <- st_read(file.path(DATA_DIR, 'Streets   Active and Retired_20240331.geojson'))
highways <- streets[streets$layer == 'FREEWAYS' & streets$active == 'true',]
highway_lines <- st_union(highways)
filtered <- st_filter(filtered, highway_lines, .predicate=st_disjoint)
nrow(filtered)

saveRDS(filtered, file.path(PROJECT_DIR, 'five_rezonings_filtered.RDS'))

}


if (sys.nframe() == 0) {
  filter_extraneous()
}
