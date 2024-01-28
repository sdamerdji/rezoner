library(sf)

sud <- st_read('./Zoning Map - Special Use Districts_20240122.geojson')
peg <- sud[sud$name == 'Priority Equity Geographies Special Use District',]$geometry
saveRDS(peg, './rezoner/peg.RDS')
