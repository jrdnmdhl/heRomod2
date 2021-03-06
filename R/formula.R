#' Define a Formula
#'
#' Takes a string representing an R-expression and turns it into a heRoFormula
#' object. If the resulting R-expression is invalid, it will be replaced
#' with an expression that evaluates to an error string.
#'
#' @param text An atomic character vector containing an R-expression.
#' @return A heRoFormula object representing the given R-expression.
#' 
#' @keywords internal
define_formula <- function(string) {
  
  # Try to capture the expression
  tryExpr <- try(
    as.lazy(string),
    silent = T
  )
  if (inherits(tryExpr, "try-error")) {
    # If the expression can't be parsed, generate an error object.
    res_list <- list(
      text = string,
      lazy = as.lazy(
        'tryCatch(stop("Error in formula syntax."' %&%
          ', call. = F), error = function(e) e)'
      ),
      err = tryExpr,
      depends = '',
      fo_depends = '',
      after = ''
    )
  } else {
    # Build data for heRoFormula object
    res_list <- list(
      text = string,
      lazy = tryExpr,
      depends = all.vars(tryExpr$expr, functions = T),
      fo_depends = all.vars(tryExpr$expr, functions = T),
      after = ''
    )
  }
  
  # Return heRoFormula object
  as.heRoFormula(res_list)
}

# Evaluate Formula
eval_formula <- function(x, ns) {
  
  # Attempt to evaluate expression
  suppressWarnings({x$lazy$env <- ns$env})
  res <- safe_eval(lazy_eval(x$lazy, data = ns$df))
  if (is_error(res)) {
    # Check if any of the variables referenced is an error 
    vars <- x$depends
    for (i in rev(vars)) {
      if (i %in% get_names(ns, 'all', keywords = F)) {
        value <- ns[i]
        if (is_error(value)) {
          error_msg <- glue('Error in dependency "{i}".')
          res <- define_error(error_msg)
        }
      }
    }
  }
  
  # Return the result
  res
}

# Safely evaluate an arbitrary statement and return the result if
# successful or an error object if not.
safe_eval <- function(x) {
  
  # Evaluate the expression
  res <- tryCatch(x, error = function(e) e, silent = T)
  
  # If an error occurs, create an error message
  if ('error' %in% class(res)) {
    res <- define_error(res)
  }
  
  # Return the result
  res
}

#' Convert a Formula to Character
#'
#' Takes a heRoFormula object and converts it to its character representation.
#'
#' @param x A heRoFormula object.
#' @param ... additional unused arguments.
#' 
#' @return An atomic character vector representing the R-expression.
#' 
#' @export
as.character.heRoFormula <- function(x, ...) {
  x$text
}

#' @export
print.heRoFormula <- function(x, ...) {
  cat(paste0('FORMULA: ', x$text))
}

#' Convert to Formula
#'
#' Convert an object to a heRovar object.
#'
#' @param x Object to be converted to heRovar.
#' @return A heRovar object.
#' 
#' @keywords internal
#' @export
as.heRoFormula <- function(x) {
  UseMethod('as.heRoFormula', x)
}

# heRoFormula => heRoFormula
#' @export
as.heRoFormula.heRoFormula <- function(x) {
  # Identity
  x
}

# character => heRoFormula
#' @export
as.heRoFormula.character <- function(x) {
  # Run define_formula on it
  define_formula(x)
}

# numeric => heRoFormula
#' @export
as.heRoFormula.numeric <- function(x) {
  # Run define_formula on it
  define_formula(as.character(x))
}

# list => heRoFormula
#' @export
as.heRoFormula.list <- function(x) {
  # Check for essential fields then set class property.
  props_to_check <- c('text', 'depends', 'lazy')
  for (prop in props_to_check) {
    if (is.null(x[[prop]])) stop(paste0('Property "', prop, '" was missing.'))
  }
  class(x) <- c('heRoFormula', 'list')
  x
}
