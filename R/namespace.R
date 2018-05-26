#' Define a Namespace Object
#'
#' Creates a new namespace object which can be used
#' to store evaluated parameters.  A namespace object
#' combines a data frame for storing vector parameters
#' with an environment for storing non-vector parameters.
#'
#' @param df A data frame of pre-existing parameter values
#' @param parent An optional environment to serve as the
#' parent to the namespace environment
#'
#' @keyword internal
define_namespace <- function(df, parent = new.env()) {
  ns <- list(
    df = df,
    env = new.env(parent = parent)
  )
  class(ns) <- "namespace"

  ns
}