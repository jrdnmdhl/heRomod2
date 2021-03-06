% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/look_up.R
\name{look_up}
\alias{look_up}
\title{Look Up Values in a Data Frame}
\usage{
look_up(data, ..., bin = FALSE, value = "value")
}
\arguments{
\item{data}{A reference data frame.}

\item{...}{Individual characteristics, should be named 
like the columns of `data`.}

\item{bin}{Either logical: should all numeric variable be
binned, or character vector giving the names of
variables to bin (see examples).}

\item{value}{The value to extract ffrom the reference 
data frame.}
}
\value{
A vector of values, same lenght as `...`.
}
\description{
A convenience function to easily look for values in a 
data frame.
}
\details{
This function is mostly used to extract population 
informations (such as mortality rates), given some 
individual caracteristics.

If binning is activated, numeric individual 
characteristics are matched to the corresponding 
reference value that is directly inferior.
}
