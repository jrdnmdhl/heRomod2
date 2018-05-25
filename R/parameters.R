
#' Evaluate Parameters
#'
#' Evaluates a parameters object and returns a table with
#' the values of each parameter in each cycle.
#'
#' @param x An object of class `param_uneval`
#' @param df A `data.frame` representing existing parameter values
#'
#' @keywords internal
eval_parameters <- function(x, df) {

}

#' Sort Parameters
#'
#' Sorts unevaluated parameters such that no parameter is
#' referenced before it is defined.  Will detect and throw
#' an error in case of circular references.
#'
#' @param x An object of class `param_uneval`
#' @return A sorted object of class `param_uneval`
#'
#' @keywords internals
sort_parameters <- function(x) {

}

#' Evaluate a Single Parameter
#'
#' Evaluates a single parameter in the context of the given
#' `data.frame` of previously evaluated parameters.
#'
#' @param x An object of class `lazy`
#' @param df A `data.frame` representing existing parameter values
#'
#' @keywords internal
eval_parameter <- function(x, df) {

}