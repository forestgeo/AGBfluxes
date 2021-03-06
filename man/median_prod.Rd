% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AGBfluxes_functions.R
\name{median_prod}
\alias{median_prod}
\title{Set mean productivity}
\usage{
median_prod(DT, flag_stranglers, exclude_interval)
}
\arguments{
\item{exclude_interval}{a vector (i.e. c(1,2)) indicating if a set of census intervals must be discarded from computation due for instance to a change in  protocol of measurment}

\item{DF}{a data.table}

\item{site}{provide the full name of your site (in lower case) i.e. 'barro colorado island'}

\item{strangler}{TRUE or FALSE, if TRUE, strangler figs tree are flagged (upon a list to published soon)}
}
\value{
mean productivity in Mg/ha/yr.
}
\description{
Set mean productivity (Mg/ha/yr) across all census intervals for a given site
}
\author{
Ervan Rutishauser (er.rutishauser@gmail.com)
}
