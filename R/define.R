#' Define Model Parameters
#'
#' Define a set of named R-expressions representing
#' model parameters.
#'
#' @param ... A set of named parameters
#'
#' @rdname define_parameters
#' @export
define_parameters <- function(...) {

  # Convert to lazy dots
  lazy_params <- lazyeval::lazy_dots(...)

  # Define parameterss
  define_parameters_(lazy_params)
}

#' @param x A data frame with columns `name` and `formula`
#'
#' @rdname define_parameters
#' @export
define_parameters_tabular <- function(x) {

  # Check column names
  if (!all(c("name", "formula") %in% colnames(x))) {
    stop("Parameters table must have columns 'name' and 'formula'")
  }

  # Convert to a named character vector
  formulas <- as.character(x$formula)
  names(formulas) <- x$name

  # Convert to lazy dots
  lazy_params <- capture_parameters(formulas)

  # Define parameters
  define_parameters_(lazy_params)
}

#' @param .dots An object of type `lazy_dots` representing
#' the unevaluated parameters.
#'
#' @rdname define_parameters
#' @keyword internal
define_parameters_ <- function(.dots) {
  class(.dots) <- append(class(.dots), "parameters")
  sort(.dots)
}

capture_parameters <- function(formulas) {
  tryCatch(
    as.lazy_dots(formulas),
    error = function(e) {
      for(i in seq_len(length(formulas))) {
        this_res <- try(as.lazy(formulas[[i]]))
        if(inherits(this_res, "try-error")) {
          error_string <- glue("Parameter '{names(formulas)[i]}' is not a valid R-Expression")
          stop(error_string, call. = F)
        }
      }
    }
  )
}