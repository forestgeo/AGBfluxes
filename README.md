
<!-- README.md is generated from README.Rmd. Please edit that file -->

# <img src="https://i.imgur.com/vTLlhbp.png" align="right" height=88 /> Compute biomass fluxes at ForestGEO sites

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build
status](https://travis-ci.org/forestgeo/AGBfluxes.svg?branch=master)](https://travis-ci.org/forestgeo/AGBfluxes)
[![CRAN
status](https://www.r-pkg.org/badges/version/AGBfluxes)](https://cran.r-project.org/package=AGBfluxes)
[![Coverage
status](https://coveralls.io/repos/github/forestgeo/AGBfluxes/badge.svg)](https://coveralls.io/r/forestgeo/AGBfluxes?branch=master)

## Main purposes
This function works either at stem (stem=T) or tree (stem=F) levels, and works in 2 steps:

### A. Complementing data
* compiles multiple censuses into a single file to,
* checks for consistency in stem/tree status (alive/dead) over time,
* (optional) fills gaps (i.e. missing DBHs or POM values) by simple linear interpolation (fill_missing=T),
* (optional) corrects POM changes through application of a taper correction (taper_correction=T),
* allocate wood density from CTFS wood density database and DRYAD
* estimates stem/tree above-ground dry biomass (AGB)

### B. Formating data
* merges information from stems to single tree for each census interval,
* codes if a tree is recruited, alive, dead or broken/resprouted
* computes annual **AGB productivity** (if alive), **ingrowth** (if recruited or resprouted) or **loss** (if dead) at tree-level
* flags obvious measurement errors (annual AGB growth > X % (X = maxrel) of mean annual AGB growth across all census intervals)


The function returns a data.frame where each row correspond to the initial and final measurments (i.e. DBH, POM, status) per **tree** for a given census intervals. Variables related to the initial and final census are denoted with **1** and **2**, respectively. Variable are defined as follow:

| Variable | Definition
---------- | -------------
treeID     |  Unique tree ID
dbh1       |  Measured dbh at intial census
dbhc1      |  Corrected dbh at intial census
status1    |  Status (alive/dead) at intial census
code1      |  Original CTFS code at intial census
hom1       |  Original height of measurement at intial census 
sp         |  CTFS species name acronym
wsg        |  Wood-density allocated at lowest taxonomic level
agb1       |  Above-ground tree biomass estimate at initial census 
date1      |  Date of census
...        |
broken     |  Has the main stem DBH > 10cm broken (i.e. AGB reduction > 20%)? 
agbl       |  AGB loss due to main stem breakage
agb1.surv  |  AGB of surving stems (if any) after main stem breakage
interval   |  first, second, third... census interval
year       |  Calendar year of census
gx         |  X coordinate
gy         |  Y coordinate
quadrat    |  20x20m quadrat
name       |  Genus and species
ID         |  Concatenation of treeID and stem tag
int        |  Census interval length in days
code       |  Corrected tree status, can be: "A" = alive, "AC" = alive, with POM changed, "B" = broken, "Rsp" = resprouted, "R" = recruited or "D" = dead
dHOM       |  hom2-hom1
prod.g     |  annual AGB productivity for trees coded as "A" or "AC"
prod.r     |  annual AGB productivity for trees coded as "Rsp" or "R"
loss       |  annual AGB loss for trees coded as "B" or "D"
ficus      |  Is that tree a large (DBH > 50cm) strangler fig?
prod.rel   |  relative producitivity (prod.g/average-productivity-per-hectare)
error      |  Is prod.rel > maxrel (1), or prod.rel < -maxrel (-1)
error.loss |  Binary. Was that tree flagged as "error" prior to death?


Resulting data set can further be used to compute AGB fluxes at a site (as described below). 

## Installation

You can install the released version of AGBfluxes from
[CRAN](https://CRAN.R-project.org) with:

``` r
# install.packages("devtools")
devtools::install_github("AGBfluxes")
```

## Run 'data_preparation'

(TO DO ER: How to locate raw data here?)

`data_preparation()` outputs a data.table, which has a special `print()`
method.

``` r
library(AGBfluxes)

# Tell the function where to locate the data
# DATA_path <- "~ path/to/your/data"

# Check that all required files are listed. The 'data' folder should contain:
1) the different stem/tree censuses described as: "site_stem1.Rdata" or "site_full1.Rdata" (for trees)
2) a list of species encountered and the CTFS acronym used.

list.files(DATA_path)

# Set the different argument as desired (detailed in ?data_prepartion) and run the function

DAT <- data_preparation(
  site = "barro colorado island",
  stem = TRUE,
  taper_correction = TRUE,
  fill_missing = TRUE,
  use_palm_allometry = TRUE,
  flag_stranglers = TRUE,
  dbh_stranglers = 500,
  maxrel = 0.2,
  write_errors_to = NULL,
  DATA_path = NULL,
  exclude_interval = NULL,
  graph_problems_to = NULL
)
#> Step 1: Data import done.
#> Step 2: Data consolidation done.
#> The reference dataset contains 16781 wood density values 
#> Your taxonomic table contains 1040 taxa
#> Step 3: AGB calculation done.
#> Step 4: Formating intervals done.
#> Step 5: Errors flagged.

head(DAT)
#>    treeID dbh1  dbhc1 status1  code1 hom1     sp       wsg       agb1
#> 1:     19  298  310.8       A B;cylY  3.0 gustsu 0.5800000  0.6595199
#> 2:     21  348  348.0       A   <NA>  1.3 virosu 0.4179091  0.6451540
#> 3:     24  438  438.0       A   <NA>  1.3 protte 0.5697500  1.5101016
#> 4:     25 1290 1420.4       A B;cylN  5.2 brosal 0.5997187 26.4205588
#> 5:     33  405  426.6       A B;cylN  3.4 guatdu 0.4660000  1.1766135
#> 6:     34 1653 1741.0       A B;cylY  3.4 anacex 0.3912857 28.6507934
#>    date1 dbh2  dbhc2 status2  code2 hom2       agb2 date2 broken agbl
#> 1: 14878  304  317.0       A B;cylY  3.0  0.6925962 16723      0   NA
#> 2: 14873  357  357.0       A   <NA>  1.3  0.6870959 16720      0   NA
#> 3: 14872  456  456.0       A   <NA>  1.3  1.6663617 16720      0   NA
#> 4: 14878 1290 1420.4       A B;cylN  5.2 26.4205588 16723      0   NA
#> 5: 14878  424  446.6       A B;cylY  3.4  1.3162106 16723      0   NA
#> 6: 14878 1656 1744.2       A B;cylY  3.4 28.7730904 16723      0   NA
#>    agb1.surv interval year    gx    gy quadrat                name
#> 1:        NA        1 2005 994.1 488.3    4924    Gustavia superba
#> 2:        NA        1 2005 990.5 488.9    4924 Virola surinamensis
#> 3:        NA        1 2005 992.7 469.3    4923 Protium tenuifolium
#> 4:        NA        1 2005 981.9 473.5    4923 Brosimum alicastrum
#> 5:        NA        1 2005 985.9 442.5    4922 Guatteria dumetorum
#> 6:        NA        1 2005 991.5 430.3    4921 Anacardium excelsum
#>           ID      int code dHOM      prod.g prod.r loss ficus   prod.rel
#> 1: 2005-4924 5.047880    A    0 0.006552515     NA   NA     0 -0.3793547
#> 2: 2005-4924 5.053352    A    0 0.008299813     NA   NA     0 -0.4805136
#> 3: 2005-4923 5.056088    A    0 0.030905336     NA   NA     0 -1.7892494
#> 4: 2005-4923 5.047880    A    0 0.000000000     NA   NA     0  0.0000000
#> 5: 2005-4922 5.047880    A    0 0.027654611     NA   NA     0 -1.6010503
#> 6: 2005-4921 5.047880    A    0 0.024227397     NA   NA     0 -1.4026334
#>    error error.loss
#> 1:    -1          0
#> 2:    -1          0
#> 3:    -1          0
#> 4:     0          0
#> 5:    -1          0
#> 6:    -1          0
```

The output is of class “data.table”.

``` r
 df[treeID==212507]

```

You can save the output to a csv file of your choice.

``` r
temp_file <- tempfile()
write.csv(prep, temp_file)
```

Then read it and re-use it as needed.

``` r
prep_2 <- read.csv(temp_file, stringsAsFactors = FALSE)
prep_2[1:5, 1:5]
#>   X treeID dbh1  dbhc1 status1
#> 1 1     19  298  310.8       A
#> 2 2     21  348  348.0       A
#> 3 3     24  438  438.0       A
#> 4 4     25 1290 1420.4       A
#> 5 5     33  405  426.6       A
```

The newly read object is no longer of class “data.table”.

(TODO: We should probably output a simple “data.frame” from
`data_preparation()`. The output is small and I see no benefit on
producing a “data.table”.)

### Side effects

TODO: a funciton should either compute something or throw side effects,
not both. We should likely exctact out the functionality that produces
side effects.

Currently `data_preparation()` can produce two side effects:

  - Write a .csv file with error flags.
  - Write .pdf file(s) showing problematic trees.

<!-- end list -->

``` r
tmp <- tempdir()

errors <- paste0(tmp, "/errors")
# You would do something like this: 
# errors <- "results/errors"

problems <- paste0(tmp, "/problems")
# You would do somethign like this: 
# problems <- "results/problems"

prep_3 <- data_preparation(
  site = "barro colorado island",
  stem = TRUE,
  taper_correction = TRUE,
  fill_missing = TRUE,
  use_palm_allometry = TRUE,
  flag_stranglers = TRUE,
  dbh_stranglers = 500,
  maxrel = 0.2,
  write_errors_to = errors,
  DATA_path = NULL,
  exclude_interval = NULL,
  graph_problems_to = problems
)
#> Step 1: Data import done.
#> Step 2: Data consolidation done.
#> The reference dataset contains 16781 wood density values 
#> Your taxonomic table contains 1040 taxa
#> Step 3: AGB calculation done.
#> Step 4: Formating intervals done.
#> Step 5: Errors flagged.
```

Show that side effects have been saved.

``` r
dir(tmp, pattern = "csv$|pdf$$")
#>  [1] "errors.csv"      "problems_1.pdf"  "problems_10.pdf"
#>  [4] "problems_11.pdf" "problems_2.pdf"  "problems_3.pdf" 
#>  [7] "problems_4.pdf"  "problems_5.pdf"  "problems_6.pdf" 
#> [10] "problems_7.pdf"  "problems_8.pdf"  "problems_9.pdf"
```

``` r
prep_3 <- read.csv(paste0(tmp, "/errors.csv"))
names(prep_3)
#>  [1] "X"          "treeID"     "dbh1"       "dbhc1"      "status1"   
#>  [6] "code1"      "hom1"       "sp"         "wsg"        "agb1"      
#> [11] "date1"      "dbh2"       "dbhc2"      "status2"    "code2"     
#> [16] "hom2"       "agb2"       "date2"      "broken"     "agbl"      
#> [21] "agb1.surv"  "interval"   "year"       "gx"         "gy"        
#> [26] "quadrat"    "name"       "ID"         "int"        "code"      
#> [31] "dHOM"       "prod.g"     "prod.r"     "loss"       "ficus"     
#> [36] "prod.rel"   "error"      "error.loss" "y1"
dim(prep_3)
#> [1]  0 39
```
