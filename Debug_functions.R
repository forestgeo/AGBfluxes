## Debug AGBfluxes functions

# load data
load("/cloud/project/data/df.rda")
load("/cloud/project/data/ficus.rda")
load("/cloud/project/data/site.info.rda")
library('AGBfluxes')
devtools::load_all()


# # Create mini-data
# # Create mini-data
grid.size=10
area =(grid.size^2)/(10000)
df <- create_quad(df,grid.size,x="gx",y="gy",fit.in.plot=T)
A <- seq(1, 500, 50)
B <- sapply(A,function(x) x:(x+9),simplify = TRUE)
bci_stem_1995 <- df[quad%in%B & year==1995 & dbh>=50,]
bci_stem_2000 <- df[quad%in%B & year==2000 & dbh>=50,]
bci_stem_2005 <- df[quad%in%B & year==2005 & dbh>=50,]

# Generate fake missing values
TREEID <- c(7519, 227275, 216886, 212363, 221761, 216782, 217266, 222175,212507, 216732)
df[treeID%in%TREEID & year==2000,c("dbh","hom"):=list(NA,NA)]
use_data(bci_stem_1995,overwrite = TRUE)
use_data(bci_stem_2000,overwrite = TRUE)
use_data(bci_stem_2005,overwrite = TRUE)

# try data_preparation()
# Debug
 site="barro colorado island";dbh_units="mm";stem=T;taper_correction=T;fill_missing=T;palm=T;flag_stranglers=T;maxrel=0.2;graph_problem_trees=F;output_errors=F;exclude_interval=0;assign("dbh_stranglers",500);use_palm_allometry=T

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

