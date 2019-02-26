#' @export
define_variable_list <- function(df) {
  params <- lapply(df$formula, define_variable)
  names(params) <- df$name
  as.heRovar_list(params)
}

as.heRovar_list

as.heRovar_list.list <- function(x) {
  if ()
  structure(x, class = 'parameters')
}

#' @export
print.parameters <- function(x, ...) {
  for (i in seq_len(length(x))) {
    cat(
      crayon::red(names(x)[i]),
      crayon::red(': '),
      crayon::black(as.character(x[[i]])),
      '\n',
      sep = ''
    )
  }
}

#' @export
`[.parameters`  <- function(x, i, ...) {
  new_parameters(NextMethod())
}

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
  
  # Extract parameter names
  par_names <- names(x)
  
  # Extract the names referenced in each parameter's
  # formula
  var_list <- purrr::map(x, function(y) {
    vars <- y$vars
    vars[vars %in% par_names]
  })
  
  # Define the lists of ordered and unordered parameters
  ordered <- c()
  unordered <- var_list
  
  # While we still have parameters in the unordered list...
  while(length(unordered) > 0) {
    
    # Define a vector which will hold the indices of each
    # parameter to be moved to the ordered list
    to_remove <- c()
    
    # Loop through each unordered parameter
    for(i in seq_len(length(unordered))) {
      
      # If all the parameters its formula references
      if(all(unordered[[i]] %in% ordered)) {
        
        # Append it to the list of ordered parameters
        ordered <- c(ordered, names(unordered)[i])
        
        # Add second-order dependencies to variable
        current_var <-  names(unordered)[i]
        x[[current_var]]$vars <- x %>%
          .[x[[current_var]]$vars] %>%
          lapply(function(x) x$vars) %>%
          purrr::discard(is.null) %>%
          purrr::flatten_chr(.) %>%
          union(x[[current_var]]$vars)
        
        # and mark it  for remval
        to_remove <- c(to_remove, i)
      }
    }
    
    if(length(to_remove) == 0) {
      # If we didn't find anything to move to the ordered list,
      # throw a circular reference error
      stop('Circular reference in parameters', call. = F)
    } else {
      # Otherwise, remove from the unordered list the parameters
      # that were appended to the ordered list
      unordered <- unordered[-to_remove]
    }
  }
  
  # Sort the parameters list according to the new order
  res <- x[ordered]
  res
  
}
