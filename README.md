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
