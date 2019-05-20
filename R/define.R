#' Capture a Variable
#'
#' Takes a string representing an R-expression and turns it into a heRovar
#' object. If the resulting R-expression is invalid, it will be replaced
#' with an expression that evaluates to an error string.
#'
#' @param text An atomic character vector containing an R-expression.
#' @return A heRovar object representing the given R-expression.
#' 
#' @keywords internal
define_variable <- function(string) {

  # Try to capture the expression
  tryExpr <- try(
    as.lazy(string),
    silent = T
  )

  # Build data for heRovar object
  if (inherits(tryExpr, "try-error")) {
    res_list <- list(
      text = string,
      lazy = as.lazy(
        'tryCatch(stop("Error in formula syntax."' %&%
        ', call. = F), error = function(e) e)'
      ),
      err = tryExpr,
      depends = '',
      after = ''
    )
  } else {
    res_list <- list(
      text = string,
      lazy = tryExpr,
      depends = all.vars(tryExpr$expr, functions = T),
      after = ''
    )
  }

  # Return heRovar object
  as.heRovar(res_list)
}

#' Capture a List of Variables
#'
#' Takes a data.frame representing a set of heRo variables and converts it to
#' a heRovar_list object.
#'
#' @param df A data.frame with columns for `name` and `formula`.
#' @return A heRovar_list object representing the given variables.
#' 
#' @keywords internal
define_variable_list <- function(df) {
  params <- lapply(df$formula, define_variable)
  names(params) <- df$name
  as.heRovar_list(params)
}
