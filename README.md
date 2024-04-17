# rezoner

Here is the minimal set of instructions to get the app running locally.

Install [git-lfs](https://docs.github.com/en/repositories/working-with-files/managing-large-files/installing-git-large-file-storage)

In the command line, run the following:
```
git lfs pull --include "rezoner/light_model.rds"
git lfs pull --include "rezoner/five_rezonings_nongeo.rds"
git lfs pull --include "rezoner/sf_map.rds"
```

This should download the required collateral. 

In RStudio run the following commands:

```
required_packages <- c("shiny", "dplyr", "sf", "shinyjs", "shinyBS", "mapboxer", "sortable", "shinyWidgets", "stringr", "viridis", "compiler")
missing_packages <- setdiff(required_packages, installed.packages()[, "Package"])

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

```

Then in RStudio, open app.R and click 'Run App' at the top right. The app should run!
