
<!-- README.md is generated from README.Rmd. Please edit that file -->

# <img src="https://i.imgur.com/vTLlhbp.png" align="right" height=88 /> Compute biomass fluxes at ForestGEO sites

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build
status](https://travis-ci.org/forestgeo/AGBfluxes.svg?branch=master)](https://travis-ci.org/forestgeo/AGBfluxes)
[![CRAN
status](https://www.r-pkg.org/badges/version/AGBfluxes)](https://cran.r-project.org/package=AGBfluxes)
[![Coverage
status](https://coveralls.io/repos/github/forestgeo/AGBfluxes/badge.svg)](https://coveralls.io/r/forestgeo/AGBfluxes?branch=master)

## Installation

You can install the released version of AGBfluxes from
[CRAN](https://CRAN.R-project.org) with:

``` r
# install.packages("devtools")
devtools::install_github("AGBfluxes")
```

## Example

``` r
# Document: devtools::document() or (Shift + Control + D)
# Build: devtools::build() or Shift + Control + B)
# Install: devtools::install() or click Install and Restart
# Restart session (Control + Shift + F10)

library(AGBfluxes)

data_preparation(
  site = "barro colorado island",
  stem = TRUE,
  taper_correction = TRUE,
  fill_missing = TRUE,
  use_palm_allometry = TRUE,
  flag_stranglers = TRUE,
  dbh_stranglers = 500,
  maxrel = 0.2,
  output_errors = TRUE,
  DATA_path = NULL,
  exclude_interval = NULL
)
#> Step 1: Data import done.
#> Error in consolidate_data(df, dbh_units, taper_correction, fill_missing, : unused argument (stem)
```
