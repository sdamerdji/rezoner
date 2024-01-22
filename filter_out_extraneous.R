df <- st_read_feather('./rezoner/four_rezonings_v3.feather')
tax <- st_read('./Assessor Historical Secured Property Tax Rolls_20240121.geojson')

parcels_to_exclude <- c('State of California Property', 'Under Water Lot')
# Get bad parcels
bad_parcels <- tax %>%
  filter((property_class_code_definition %in% parcels_to_exclude) | 
           (exemption_code_definition == 'Cemetary') | (block == '9900') |
           (block == '0006' & lot == '001'))

# Take union of points of bad parcels
to_exclude <- st_union(bad_parcels)

# Remove from df
filtered <- st_filter(df, to_exclude, .predicate=st_disjoint)
nrow(filtered)

# Check that none are being upzoned already
removed <- st_filter(df, to_exclude)
sum(!is.na(removed$M1_ZONING))
sum(!is.na(removed$M2_ZONING))
sum(!is.na(removed$M3_ZONING))
sum(!is.na(removed$M4_ZONING)) # Ok so one parcel is wrongly removed. need to add back after the fact

st_write_feather(filtered, './rezoner/four_rezonings_v4.feather')

colnames(tax)
results <- st_join(df, tax)
