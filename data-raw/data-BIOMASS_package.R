# Datasets from BIOMASS ---------------------------------------------------

# FIXME: Once we know precisely what data we need, use just that.

# Some datasets are used by and documented in BIOMASS but not exported. We
# download the source code from CRAN stored all data in dataa-raw/ (source:
# https://cran.r-project.org/web/packages/BIOMASS/index.html). To get the code
# up and runnig quickly I'll include **all** those datasets in this package.
# This is bad practice and we should fix it. In the meantime, to contain the
# risk of problems within this package, I'll make this data internal.

# Load all data from BIOMASS
biomass_data <- "data-raw/BIOMASS_1.2/BIOMASS/data"
purrr::walk(fs::dir_ls(biomass_data), ~load(.x, envir = globalenv()))

usethis::use_data(
  apgFamilies,
  feldCoef,
  genusFamily,
  KarnatakaForest,
  NouraguesHD,
  param_4,
  param_7,
  sd_10,
  wdData,
  internal = TRUE,
  overwrite = TRUE
)
