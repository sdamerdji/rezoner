######### Load libraries ##########
library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(modelr)
library(caret)
library(sf)
library(sfarrow)

######### Create a dataset for the Fall 2023 rezoning #############
map4 <- st_read('~/Desktop/rezoner2/Fall2023Rezoning.json', promote_to_multi=F)
# The above dataset lacks info on the fourplex rezoning, so I have to separately
# load the block-level dataset on where fourplexes are allowed now.
fourplex <- st_read('~/Desktop/rezoner2/fourplex_areas_12_2023.geojson')
fourplex <- st_transform(fourplex, st_crs(map4))

map4 <- map4 %>%
  mutate(M4_ZONING = ifelse(grepl("Unchanged", HeightText), NA, HeightText)) %>%
  rename(height = HeightInteger) %>%
  mutate(ACRES = Shape__Area / 43560)

# Incorporate fourplex zoning to Fall 2023 rezoning dataset
fourplex_description <- "Increased density up to four units (six units on corner lots)"
intersections <- st_join(map4[is.na(map4$M4_ZONING),], fourplex, left=F)
map4$M4_ZONING[
  (map4$mapblklot %in% intersections$mapblklot) 
  & is.na(map4$M4_ZONING)
  ] <- fourplex_description


######### Load and clean sites inventory dataset #############
sf_sites_inventory <- read_excel("~/Desktop/rezoner2/sf-sites-inventory-form-Dec-2022 - Copy.xlsx", 
                                 sheet = "Table B -Submitted-Dec-22", 
                                 skip=1, 
                                 na='n/a')
# I need a spatial join to merge the Fall 2023 dataset with the sites inventory 
# dataset. Hence, I'm first joining the sites inventory with a geospatial 
# bluesky dataset.
sf_history <- st_read('~/Desktop/rezoner2/geobluesky.geojson')
sf_history <- st_transform(sf_history, st_crs(map4))

# Clean and join
sf_sites_inventory <- mutate(sf_sites_inventory,
             MapBlkLot_Master = stringr::str_replace(mapblklot, "-", ""),
             year = 2016) %>%
  select(-mapblklot) %>%
  distinct(MapBlkLot_Master, .keep_all = TRUE)

# Add blue sky variables to sf_sites_inventory
df <- sf_history %>%
  filter(year == 2016) %>%
  left_join(sf_sites_inventory, by = c('MapBlkLot_Master', "year")) %>%
  st_sf()

# Dropping 20 needless columns
df <- df[ , !grepl("(_VLI|_LI|_M|_AM)$", colnames(df))]
to_drop <- c('JURISDICT', 'ZIP5', 'SHORTFALL', 'MIN_DENS', 
             'SS_MAP1', 'SS_MAP2', 'SS_MAP3', 'INFRA')
df <- df[ , !(colnames(df) %in% to_drop)]

######### Merge sites inventory and fall 2023 rezoning dataframes #########
# Merge first on mapblklot where possible
merge1 <- inner_join(map4, 
                     st_drop_geometry(df),
                     'mapblklot')
nrow(merge1)
nrow(map4)

# Merge remaining spatially
unmerged <- map4[!(map4$mapblklot %in% merge1$mapblklot),]
merge2 <- st_join(df[!(df$mapblklot %in% merge1$mapblklot),],
                  unmerged,
                  left='F')

# I assume other parcels are primarily fourplexes... but no
unmerged2 <- unmerged[!(unmerged$mapblklot %in% merge2$mapblklot),]
table(unmerged2$M4_ZONING)

# Merge unmerged2 with sf_history filtered for year 2016
# Lacks site inventory column headers
merge3 <- sf_history %>%
  st_drop_geometry() %>%
  inner_join(unmerged2, by = c('MapBlkLot_Master' = 'mapblklot')) %>%
  group_by(mapblklot) %>%
  slice_max(order_by = year, with_ties = FALSE) %>%
  st_sf()

unmerged3 <- unmerged2[!(merge3$mapblklot %in% merge3$mapblklot),]
merge4 <- st_join(df, unmerged3,  left='F')
unmerged4 <- unmerged3[!(unmerged3$mapblklot %in% merge4$mapblklot.y),]

merge1 <- merge1 %>% 
  rename(ACRES=ACRES.x) %>%
  select(-ACRES.y)
merge2 <- merge2 %>% 
  rename(ACRES=ACRES.x, mapblklot=mapblklot.x) %>%
  select(-ACRES.y, -mapblklot.y) 
merge4 <- merge4 %>% 
  rename(ACRES=ACRES.x, mapblklot=mapblklot.x) %>%
  select(-ACRES.y, -mapblklot.y)

final <- rbind(merge1, merge2, merge4)
final <- plyr::rbind.fill(merge3, final)

# In sites inventory maps, where zoning is NA, indicate it's
# fourplex if it's fourplex in Fall 2023 rezoning
final[is.na(final$M1_ZONING) 
      & !is.na(final$M4_ZONING)
      & (final$M4_ZONING == fourplex_description),
      "M1_ZONING"] <- fourplex_description
final[is.na(final$M2_ZONING) 
      & !is.na(final$M4_ZONING)
      & (final$M4_ZONING == fourplex_description),
      "M2_ZONING"] <- fourplex_description
final[is.na(final$M3_ZONING) 
      & !is.na(final$M4_ZONING)
      & (final$M4_ZONING == fourplex_description),
      "M3_ZONING"] <- fourplex_description


final <- st_sf(final)
#st_write(final, "~/Desktop/rezoner2/four_rezonings.geojson")
final <- st_transform(final[1:nrow(final),], crs = 4326)
st_write_feather(final, "~/Desktop/rezoner2/rezoner/four_rezonings.feather")
#df <- st_read_feather("~/Desktop/rezoner2/rezoner/four_rezonings.feather")

