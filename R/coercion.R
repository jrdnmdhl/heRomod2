#' Convert a Variable to Character
#'
#' Takes a heRovar object and converts it to its character representation.
#'
#' @param x A heRovar object.
#' @param ... additional unused arguments.
#' 
#' @return An atomic character vector representing the R-expression.
#' 
#' @export
as.character.heRovar <- function(x, ...) {
  x$text
}

#' Convert to Variable
#'
#' Convert an object to a heRovar object.
#'
#' @param x Object to be converted to heRovar.
#' @return A heRovar object.
#' 
#' @keywords internal
as.heRovar <- function(x) {
  UseMethod('as.heRovar', x)
}

# heRovar => heRovar
as.heRovar.heRovar <- function(x) {
  # Identity
  x
}

# character => heRovar
as.heRovar.character <- function(x) {
  # Run define_variable on it
  define_variable(x)
}

# list => heRovar
as.heRovar.list <- function(x) {
  # Check for essential fields then set class property.
  props_to_check <- c('text', 'vars', 'lazy')
  for (prop in props_to_check) {
    if (is.null(x[[prop]])) stop(paste0('Property "', prop, '" was missing.'))
  }
  structure(x, class = 'heRovar')
}

# lazy => heRovar
as.heRovar.lazy <- function(x) {
  # Pull the necessary info from lazy object into list, then convert to
  # heRovar.
  list_obj <- list(
    text = as.character(x)[1],
    lazy = x,
    vars = all.vars(x$expr, functions = T)
  )
  as.heRovar(list_obj)
}

#' @export
as.heRovar_list <- function(x) {
  UseMethod('as.heRovar_list', x)
}

#' @export
as.heRovar_list.list <- function(x) {
  structure(x, class = 'heRovar_list')
}
