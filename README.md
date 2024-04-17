# rezoner

Here is the minimal set of instructions to get the app running locally.

Install (git-lfs)[https://docs.github.com/en/repositories/working-with-files/managing-large-files/installing-git-large-file-storage]


In the command line, run the following:
```
git lfs pull
```

This should download the required collateral. To run app.R, you need at minimium the files light_model.rds, five_rezonings_nongeo.RDS, sf_map.RDS

In RStudio run the following commands:

```
required_packages <- c("shiny", "dplyr", "sf", "shinyjs", "shinyBS", "mapboxer", "sortable", "shinyWidgets", "stringr", "viridis", "compiler")
missing_packages <- setdiff(required_packages, installed.packages()[, "Package"])

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

```

Then in RStudio, open app.R and click 'Run App' at the top right. The app should run!
