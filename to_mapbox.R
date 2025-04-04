library(sf)
library(dplyr)
library(sfarrow)
library(mapboxer)
library(rmapshaper)
source('./constants.R')

to_mapbox <- function() {
df <- readRDS(file.path(PROJECT_DIR, 'five_rezonings_filtered.RDS'))

df2 <- st_set_precision(df, 10^6)
st_write_feather(df2, 'df_less_precise.feather')
df2_read <- st_read_feather('df_less_precise.feather')
saveRDS(df2_read, 'df_less_precise.RDS')
gc()

df <- readRDS('df_less_precise.RDS') # This is 1% faster (according to microbenchmark)
df <- df[!st_is_empty(df),]
simple_df <- df[df$ACRES > 0.0023 * 10,] # To allow geometry simplifications for a faster map, I do not plot lots smaller than 500ft^2

#simple_df <- ms_simplify(to_plot) #0.0495)
slim_df <- dplyr::select(simple_df, mapblklot, geometry)
df_mapbox <-  as_mapbox_source(slim_df, tolerance=.4) # only works if sf loaded

file.remove('df_less_precise.feather')
file.remove('df_less_precise.RDS')
saveRDS(df_mapbox, file.path(APP_DIR, 'sf_map.RDS'))

saveRDS(st_drop_geometry(df), file.path(APP_DIR, 'five_rezonings_nongeo.RDS'))
}


if (sys.nframe() == 0) {
  to_mapbox()
}
