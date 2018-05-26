#' Sort Model Parameters
#'
#' Sorts a parameters object in order to solve issues
#' of dependency-resolution and identify any circular
#' references.
#'
#' @param x The parameters object to be sorted
#' @param ... Unused parameters to match sort call
#' signature
#'
#' @keyword internal
sort.parameters <- function(x, ...) {
  par_names <- names(x)
  var_list <- purrr::map(x, function(y) {
    vars <- all.vars(y$expr)
    vars[vars %in% par_names]
  })
  ordered <- c()
  unordered <- var_list
  while(length(unordered) > 0) {
    to_remove <- c()
    for(i in seq_len(length(unordered))) {
      if(all(unordered[[i]] %in% ordered)) {
        ordered <- c(ordered, names(unordered)[i])
        to_remove <- c(to_remove, i)
      }
    }
    if(length(to_remove) == 0) {
      stop('Circular reference in parameters', call. = F)
    } else {
      unordered <- unordered[-to_remove]
    }
  }
  res <- x[ordered]
  class(res) <- class(x)
  res

}