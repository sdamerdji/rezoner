library(dplyr)
library(sf)
# Run this script from rezoner subdirectory
setwd('~/Desktop/rezoner2/rezoner')
df <- readRDS('./four_rezonings_v4.RDS')
tax <- st_read('../Assessor Historical Secured Property Tax Rolls_20240121.geojson')

parcels_to_exclude <- c('State of California Property', 'Under Water Lot')
# Get bad parcels
bad_parcels <- tax %>%
  filter((property_class_code_definition %in% parcels_to_exclude) | 
           (exemption_code_definition == 'Cemetary') | (block == '9900') |
           (block == '0006' & lot == '001') | (block == '1300' & lot == '001'))#  | 
           #(block == '1145' & lot == '003') | (block == '1107' & lot=='008'))

# Take union of points of bad parcels
to_exclude <- st_union(bad_parcels)

# Remove from df
filtered <- st_filter(df, to_exclude, .predicate=st_disjoint)
nrow(filtered)

filtered <- filtered[filtered$ex_height2024 < 1111,]

# Check that none are being upzoned already
# removed <- st_filter(df, to_exclude)
# sum(!is.na(removed$M1_ZONING))
# sum(!is.na(removed$M2_ZONING))
# sum(!is.na(removed$M3_ZONING))
# sum(!is.na(removed$M4_ZONING)) # Ok so one parcel is wrongly removed. need to add back after the fact


colnames(filtered) 
# Neither select matters anymore? of these s
filtered <- filtered %>% select(-MapBlkLot_Master, -Developed,
              -M4_height, -M5_height, -EX_USE,
              -M1_CAP, -M2_CAP, -M3_CAP,
              -M1_GP_TYPE, -M2_GP_TYPE, -M3_GP_TYPE, -VACANT, -primary_key)
filtered <- filtered %>% select(-sup_dist_name, -EX_GP_TYPE, -EX_ZONING)


# Remove pipeline
pipeline <- st_read('../SF Development Pipeline 2023 Q3_20240203.geojson')
pipeline <- pipeline[!is.na(pipeline$proposed_units) 
                      & (as.numeric(pipeline$proposed_units) > 0)
                      & !(is.na(pipeline$net_pipeline_units)) 
                      & (as.numeric(pipeline$net_pipeline_units) > 0)
                      & !(st_is_empty(pipeline)),]
pipeline_points <- st_union(pipeline)
result <- st_filter(filtered, pipeline_points, .predicate=st_disjoint)
nrow(result)

fourplex <- 'Increased density up to four units (six units on corner lots)'
result[!is.na(result$M1_ZONING) & (result$M1_ZONING == fourplex) & result$ex_height2024 > 65, 'M1_ZONING'] <- NA
result[!is.na(result$M2_ZONING) & (result$M2_ZONING == fourplex) & result$ex_height2024 > 65, 'M2_ZONING'] <- NA
result[!is.na(result$M3_ZONING) & (result$M3_ZONING == fourplex) & result$ex_height2024 > 65, 'M3_ZONING'] <- NA
result[!is.na(result$M4_ZONING) & (result$M4_ZONING == fourplex) & result$ex_height2024 > 65, 'M4_ZONING'] <- NA
result[!is.na(result$M5_ZONING) & (result$M5_ZONING == fourplex) & result$ex_height2024 > 65, 'M5_ZONING'] <- NA
st_write_feather(result, './four_rezonings_v5.feather')
saveRDS(result, './four_rezonings_v5.RDS')
