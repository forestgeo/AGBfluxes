% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AGBfluxes_functions.R
\name{flag_errors}
\alias{flag_errors}
\title{Flag major errors}
\usage{
flag_errors(DF, site, flag_stranglers, maxrel, write_errors_to,
  exclude_interval, graph_problems_to)
}
\arguments{
\item{DF}{a data.table}

\item{site}{provide the full name of your site (in lower case) i.e. 'barro colorado island'}

\item{maxrel}{a numeric value setting the threshold over which relative productivity is assumed to be too high (usually set at 20 percents)}

\item{exclude_interval}{a vector (i.e. c(1,2)) indicating if a set of census intervals must be discarded from computation due for instance to a change in  protocol of measurment}

\item{strangler}{TRUE or FALSE, if TRUE, strangler figs tree are flagged (upon a list to published soon)}

\item{output_errors}{TRUE or FALSE, output all records for trees with major errors in productivity to a csv file}
}
\value{
a data.table (data.frame) with all relevant variables.
}
\description{
Identify trees with major errors in DBH measurments. A major error correspond to a relative individal productivity (or growth) is above a given percentage (set by 'maxrel') of the mean productivity computed at a site. Additionnaly, flagged trees that died at next census interval are also flagged. Option to see DBH measurement (=draw.graph) of flagged trees or print a csv (output_errors) are given.
}
\author{
Ervan Rutishauser (er.rutishauser@gmail.com)
}
