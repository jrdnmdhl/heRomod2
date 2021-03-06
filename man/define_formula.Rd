% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/formula.R
\name{define_formula}
\alias{define_formula}
\title{Define a Formula}
\usage{
define_formula(string)
}
\arguments{
\item{text}{An atomic character vector containing an R-expression.}
}
\value{
A heRoFormula object representing the given R-expression.
}
\description{
Takes a string representing an R-expression and turns it into a heRoFormula
object. If the resulting R-expression is invalid, it will be replaced
with an expression that evaluates to an error string.
}
\keyword{internal}
