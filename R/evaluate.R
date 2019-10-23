


#' Evaluate a Variable
#'
#' Takes a heRovar object and converts it to its character representation.
#'
#' @param x A heRovar object.
#' @param ns A namespace object containing evaluated variables that the heRovar
#' object may reference.
#' 
#' @return The result of evaluating the heRovar object.
#' 
#' @keywords internal
evaluate_variable_list <- function(x, ns, ...) {
  
  # Iterate over each parameter and its name
  walk2(names(x), x, function(name, value) {
    
    # Evaluate it
    res <- evaluate_variable(value, ns)
    
    # Determine whether result is a vector or object parameter
    vector_type <- is.vector(res) && !is.list(res)
    if (vector_type && (length(res) == nrow(ns$df))) {
      # If a vector parameter, assign to data frame
      ns$df[name] <<- res
    } else {
      # If an object parameter, assign to environment
      assign(name, res, envir = ns$env)
    }
  })
  
  return(ns)
}
