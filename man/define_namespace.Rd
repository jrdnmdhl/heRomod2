% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/namespace.R
\name{define_namespace}
\alias{define_namespace}
\title{Define a Namespace Object}
\usage{
define_namespace(env, df, additional = NULL)
}
\arguments{
\item{env}{An environment of pre-existing values}

\item{df}{A data frame of pre-existing parameter values}
}
\description{
Creates a new namespace object which can be used
to store evaluated parameters.  A namespace object
combines a data frame for storing vector parameters
with an environment for storing non-vector parameters.
}
