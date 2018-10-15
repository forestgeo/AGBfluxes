# Load all datasets in data-raw/
paths <- fs::dir_ls(here::here("data-raw"), glob = "*.rda")
purrr::map(paths, load, envir = globalenv())

# Source: Ervan Rutishauser (er.rutishauser@gmail.com). Documented in R/data.R

# Use data (compress and move each dataset to data/)
dfm <- DF
use_data(
  bci.spptable,
  bci_stem_1995,
  bci_stem_2000,
  bci_stem_2005,
  ficus,
  site.info,
  WSG,
  dfm,
  overwrite = TRUE
)
