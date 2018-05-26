#' Perform Error Checking
#'
#' Performs error checking on a given model
#' object.
#'
#' @param x The object to be checked
#' @param ... Additional arguments to be handled
#' by the method
#'
#' @keyword internal
check <- function(x, ...) {
  UseMethod("check", x)
}