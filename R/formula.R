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
      after = ''
    )
  } else {
    # Build data for heRoFormula object
    res_list <- list(
      text = string,
      lazy = tryExpr,
      depends = all.vars(tryExpr$expr, functions = T),
      after = ''
    )
  }
  
  # Return heRoFormula object
  as.heRoFormula(res_list)
}

# Evaluate Formula
evaluate_formula <- function(x, ns) {
  
  # Attempt to evaluate expression
  suppressWarnings({x$env <- ns$env})
  res <- tryCatch(
    {
      x$lazy$env <- ns$env
      lazy_eval(x$lazy, data = ns$df)
    },
    error = function(e) {
      # Check if any of the variables referenced is an error 
      vars <- x$depends
      for (i in rev(vars)) {
        if (i %in% get_names(ns, 'all', keywords = F)) {
          value <- ns[i]
          if (class(value) == 'heRo_error') {
            error_msg <- glue('Error in dependency "{i}".')
            return(tryCatch(stop(error_msg, call. = F), error = function(e) e))
          }
        }
      }
      return(e)
    },
    silent = T
  )
  
  # If an error occurs, return error message
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

# lazy => heRoFormula
#' @export
as.heRoFormula.lazy <- function(x) {
  # Pull the necessary info from lazy object into list, then convert to
  # heRoFormula.
  list_obj <- list(
    text = as.character(x)[1],
    lazy = x,
    vars = all.vars(x$expr, functions = T),
    after = ''
  )
  as.heRoFormula(list_obj)
}
