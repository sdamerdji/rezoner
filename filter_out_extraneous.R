library(dplyr)
library(sf)
# Run this script from rezoner subdirectory
setwd('~/Desktop/rezoner2/rezoner')
df <- st_read_feather('../four_rezonings_v3.feather')
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

st_write_feather(filtered, './four_rezonings_v4.feather')
df <- st_read_feather('./four_rezonings_v4.feather')
colnames(df)
df <- df %>% select(-MapBlkLot_Master, -Developed,
              -M4_height, -M5_height, -EX_USE,
              -M1_CAP, -M2_CAP, -M3_CAP,
              -M1_GP_TYPE, -M2_GP_TYPE, -M3_GP_TYPE, -VACANT, -primary_key)
df <- df %>% select(-sup_dist_name, -EX_GP_TYPE, -EX_ZONING)
st_write_feather(filtered, './four_rezonings_v4.feather')
