# rezoner

### Run the app locally
Here is the minimal set of instructions to get the app running locally.

In RStudio run the following commands:

```
required_packages <- c("shiny", "dplyr", "sf", "shinyjs", "shinyBS", "mapboxer", "sortable", "shinyWidgets", "stringr", "viridis", "compiler", "RColorBrewer")
missing_packages <- setdiff(required_packages, installed.packages()[, "Package"])

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

```

Then in RStudio, open app.R and click 'Run App' at the top right. The app should run!

### Reproduce collateral for web app

More libraries are needed to run through the whole pipeline to re-create the pipeline that outputs light_model.rds, sf_map.rds, and five_rezonings_non_geo.rds

First you'll need to install [git-lfs](https://docs.github.com/en/repositories/working-with-files/managing-large-files/installing-git-large-file-storage)

In the command line, run the following:
```
git lfs pull
```

Then, after installing the relevant R packages, you'll source main.R.

### Make changes to the web app

Most of the logic in this web app occurs in update_df_() in app.R. If you want to change how the effect of rezonings are calculated, this is the function to change.

If you want to define a custom rezoning with greater resolution than the UI allows, look at the function yimbycity() for a pattern to borrow. In particular, you can zone at the parcel level by using the block/lot combination of parcels, which appear in the popup when you click the parcel on the map.

The web app can easily be debugged by placing the line "browser()" at a line in the code where you'd like to set a breakpoint.

