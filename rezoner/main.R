setwd(dirname(this.path::here()))  # Set wd to project root
source('./constants.R')
source(file.path(PROJECT_DIR, 'merge_scenario_d.R'))
source(file.path(PROJECT_DIR, 'preprocessing.R'))
source(file.path(PROJECT_DIR, 'filter_out_extraneous.R'))
source(file.path(PROJECT_DIR, 'to_mapbox.R'))

merge_scenario()
gc()
print("Done merging scenario")

preprocess()
gc()
print("Done preprocessing")

filter_extraneous()
gc()
print("Done filtering extraneous")

to_mapbox()
gc()
print("Done converting to mapbox")
