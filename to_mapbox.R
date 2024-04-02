library(sf)
library(dplyr)
library(sfarrow)
library(mapboxer)
library(rmapshaper)

setwd('~/Desktop/rezoner/rezoner')

df <- readRDS('../five_rezonings_filtered.RDS')

df2 <- st_set_precision(df, 10^6)
st_write_feather(df2, 'df_less_precise.feather')
df2_read <- st_read_feather('df_less_precise.feather')
saveRDS(df2_read, 'df_less_precise.RDS')

df <- readRDS('df_less_precise.RDS') # This is 1% faster (according to microbenchmark)
df <- df[!st_is_empty(df),]
to_plot <- df[df$ACRES > 0.0023 * 10,] # To allow geometry simplifications for a faster map, I do not plot lots smaller than 500ft^2

simple_df <- ms_simplify(to_plot, keep_shapes=T, keep=0.042) #0.0495)
slim_df <- dplyr::select(simple_df, mapblklot, geometry)
df_mapbox <-  as_mapbox_source(slim_df, tolerance=.475) # only works if sf loaded

file.remove('df_less_precise.feather')
file.remove('df_less_precise.RDS')
saveRDS(df_mapbox, './sf_map.RDS')

saveRDS(st_drop_geometry(df), './five_rezonings_nongeo.RDS')
