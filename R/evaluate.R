#' Evaluate Model Parameters
#'
#' Evaluates an object containing one or more
#' R-expressions representing model calculations.
#'
#' @param x The object to be evaluated
#' @param ns The namespace object in which to evaluate
#' @param ... Optional arguments to be handled by method
#'
#' @rdname evaluate
#' @export
evaluate <- function(x, ns, ...) {
  UseMethod("evaluate", x)
}

#' @export
evaluate.lazy <- function(x, ns, ...) {

  # Attempt to evaluate expression
  res <- evaluate_in_ns(x, ns)

  # If an error occurs, send error message
  if ('error' %in% class(res)) {
    stop(substring(res, 38))
  }

  # Return the result
  return(res)
}

#' @export
evaluate.parameters <- function(x, ns, ...) {

  # Iterate over each parameter and its name
  walk2(names(x), x, function(name, value) {

    # Try to Evaluate
    res <- tryCatch(
      evaluate(value, ns),
      error = function(e) e,
      silent = T
    )

    # If an error occurs, send error message
    if ('error' %in% class(res)) {
      stop(res, call. = F)
    }

    # Determine whether result is a vector or object parameter
    vector_type <- is.vector(res) && !is.list(res)
    if (vector_type && (length(res) == 1 || length(res) == nrow(ns$df))) {
      # If a vector parameter, assign to data frame
      ns$df[name] <<- res
    } else {
      # If an object parameter, assign to environment
      assign(name, res, envir = ns$env)
    }
  })

  return(ns)
}

#' @rdname evaluate
#' @keyword internal
evaluate_in_ns <- function(x, ns, ...) {
  x$env <- ns$env
  tryCatch(
    lazy_eval(x, data = ns$df),
    error = function(e) e,
    silent = T
  )
}