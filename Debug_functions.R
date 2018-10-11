## Debug AGBfluxes functions

# load data
load("/cloud/project/data/df.rda")
load("/cloud/project/data/ficus.rda")
load("/cloud/project/data/site.info.rda")
library('AGBfluxes')
devtools::load_all()

# try data_preparation()

DF <- data_preparation(
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

