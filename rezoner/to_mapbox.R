library(sf)
library(dplyr)
library(sfarrow)

setwd('~/Desktop/rezoner2/rezoner')

df <- readRDS('./four_rezonings_v5.RDS')

df2 <- st_set_precision(df, 10^6)
st_write_feather(df2, 'df_less_precise.feather')
df2_read <- st_read_feather('df_less_precise.feather')
saveRDS(df2_read, 'df_less_precise.RDS')

df <- readRDS('df_less_precise.RDS') # This is 1% faster (according to microbenchmark)
to_plot <- df[df$ACRES > 0.00229568,] # To allow simplifications. 100ft^2
library(rmapshaper)
simple_df <- ms_simplify(to_plot, keep_shapes=T, keep=0.0495)
slim_df <- dplyr::select(simple_df, mapblklot, geometry)
df_mapbox <-  as_mapbox_source(slim_df, tolerance=.2) # only works if sf loaded
saveRDS(df_mapbox, 'sf_map.RDS')

saveRDS(st_drop_geometry(df), './four_rezonings_v6.RDS')
