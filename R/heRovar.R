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
      lazy = as.lazy('"#ERR: Invalid Expression!"'),
      err = tryExpr,
      vars = ''
    )
  } else {
    res_list <- list(
      text = string,
      lazy = tryExpr,
      vars = all.vars(tryExpr$expr, functions = T)
    )
  }

  # Return heRovar object
  as.heRovar(res_list)
}

#' Convert to Variable
#'
#' Takes a list containing the elements of a heRovar object and returns a
#' heRovar object.
#'
#' @param x Object to be converted to heRovar.
#' @return A heRovar object.
#' 

#' Convert Variables Object to Character
#'
#' Takes a heRovar object and converts it to its character representation.
#'
#' @param x A heRovar object.
#' @return An atomic character vector representing the R-expression.
#' 
#' @export
as.character.heRovar <- function(x, ...) {
  x$text
}

#' Print a Variable
#'
#' Takes a heRovar object and prints it.
#'
#' @param x A heRovar object.
#' @param ... additional arguments passed to `cat`.
#' @return An atomic character vector representing the R-expression.
#' 
#' @export
print.heRovar <- function(x, ...) {
  cat(as.character(x), ...)
}

## as.heRovar methods

as.heRovar.heRovar <- function(x) {
  # Identity
  x
}

as.heRovar.character <- function(x) {
  # Run define_variable on it
  define_variable(x)
}

as.heRovar.list <- function(x) {
  # Check for essential fields then set class property.
  props_to_check <- c('text', 'vars', 'lazy')
  for (prop in props_to_check) {
    if (is.null(x[[prop]])) stop(paste0('Property "', prop, '" was missing.'))
  }
  structure(x, class = 'heRovar')
}

as.heRovar.lazy <- function(x) {
  list_obj <- list(
    text = as.character(x)[1],
    lazy = x,
    vars = all.vars(testit$expr, functions = T)
  )
  as.heRovar(list_obj)
}
