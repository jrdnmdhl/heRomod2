# Parses a variables input table and optional trees table
# and produces an object of class uneval_variables
parse_variables <- function(x, segment, trees = NULL, formula_column = 'formula') {
  
  # Filter to only variables in this segment
  df <- filter(
    x,
    is_in_segment(segment, strat = strategy, grp = group),
    !.data$name %in% names(segment)
  )
  
  # Check that variables definition is valid
  check_variables_df(df)
  
  # Parse decision tree variables
  tree_vars <- parse_tree_vars(trees)
  
  # Parse formulas, combine with trees, and sort
  vars <- df %>%
    select(name, display_name, description, !!enquo(formula_column)) %>%
    mutate(!!formula_column := map(!!sym(formula_column), as.heRoFormula)) %>%
    rbind(tree_vars) %>%
    sort_variables()
  
  # Construct Object & Return
  as.variables(vars)
}

# Sort a dataframe of variables based on dependency tree
sort_variables <- function(x, extra_vars = NULL) {
  
  # Deal with extra vars if given
  if (is.null(extra_vars)) {
    extras <- list()
  } else {
    extras <- set_names(extra_vars$formula, extra_vars$name)
  }
  
  # Extract variable names
  par_names <- x$name
  
  # Extract the variable names referenced in each variable's
  # formula
  var_list <-  purrr::map(x$formula, function(y) {
      vars <- c(y$depends, y$after)
      vars[vars %in% par_names]
    }) %>%
    set_names(x$name)
  
  # Define the lists of ordered and unordered variables
  ordered <- c()
  unordered <- var_list
  
  # While we still have variables in the unordered list...
  while (length(unordered) > 0) {
    
    # Define a vector which will hold the indices of each
    # variable to be moved to the ordered list
    to_remove <- c()
    
    # Loop through each unordered variable
    for (i in seq_len(length(unordered))) {
      
      # If all the variables its formula references are in the ordered
      # list then it can be added to ordered list.
      if (all(unordered[[i]] %in% ordered)) {
        
        # Append it to the list of ordered variables
        ordered <- c(ordered, names(unordered)[i])
        
        # Get current variable
        current_var <-  names(unordered)[i]
        
        # Make a named list of all variables
        all_vars <- c(
          set_names(x$formula, x$name),
          extras
        )
        
        # Extract any decision tree probability calls
        p_calls <- extract_func_calls(x$formula[[which(x$name == current_var)]]$lazy$expr, 'p')
        tree_deps <- map(p_calls, function(y) {
          referenced_nodes <- all_vars[[y$arg2]]$node_depends %>%
            keep(~any(.$tags %in% y$arg1)) %>%
            map(~.$depends) %>%
            flatten_chr()
          
          referenced_nodes
        }) %>% flatten_chr()
        
        # Assemble first-order depencies
        fo_deps <- unique(c(x$formula[[which(x$name == current_var)]]$depends, tree_deps))
        
        # Add second+ order dependencies to variable
        x$formula[[which(x$name == current_var)]]$depends <- x$formula %>%
          set_names(x$name) %>%
          c(extras) %>%
          .[fo_deps] %>%
          lapply(function(y) y$depends) %>%
          discard(is.null) %>%
          flatten_chr(.) %>%
          union(fo_deps)
        
        # and mark it  for removal
        to_remove <- c(to_remove, i)
      }
    }
    
    if (length(to_remove) == 0) {
      # If we didn't find anything to move to the ordered list,
      # throw a circular reference error
      stop('Circular reference in parameters', call. = F)
    } else {
      # Otherwise, remove from the unordered list the variables
      # that were appended to the ordered list
      unordered <- unordered[-to_remove]
    }
  }
  
  # Return variables in sorted order
  as_tibble(x[order(factor(x$name, levels = ordered)), ])
}

# Evaluate a variables object
eval_variables <- function(x, ns, df_only = F) {
  
  # Keep list of parameters that generated errors
  error_params <- c()
  
  # Iterate over each parameter and its name
  walk2(x$name, x$formula, function(name, value) {
    
    # Evaluate it
    res <- evaluate_formula(value, ns)
    
    # Check if the object was an error
    if (class(res) == 'heRo_error') {
      error_params <<- append(error_params, name)
    }
    
    # Determine whether result is a vector or object parameter
    vector_type <- is.vector(res) && !is.list(res)
    if (df_only || (vector_type && (length(res) == nrow(ns$df)))) {
      # If a vector parameter, assign to data frame
      ns$df[name] <<- res
    } else {
      # If an object parameter, assign to environment
      assign(name, res, envir = ns$env)
    }
  })
  
  if (length(error_params) > 0) {
    warning(
      'Error in evaluation of parameters: ',
      paste0('"', error_params, '"', collapse = ', '),
      "."
    )
  }
  
  return(ns)
}

# Checks that a variables definition table is valid
check_variables_df <- function(x) {
  
  error_msg = ''
  
  # Check that all necessary columns are present
  missing_cols <- check_missing_colnames(x, vars_def_columns)
  if (length(missing_cols) > 0) {
    plural <- if (length(missing_cols) > 1) 's' else ''
    missing_msg <- paste(missing_cols, collapse = ', ')
    error_msg <- glue('Variables definition was missing column{plural}: {missing_msg}.')
  }
  
  # Check that there are no duplicate names
  dupe <- duplicated(x$name)
  if (any(dupe)) {
    dupe_names <- unique(x$name[dupe])
    plural <- if (length(dupe_names) > 1) 's' else ''
    dupe_msg <- paste(dupe_names, collapse = ', ')
    error_msg <- glue('Variables definition contained invalid variable name{plural}: {dupe_msg}.\nName already used.')
  }
  
  # Check that variable names are valid
  checked_names <- make.names(x$name)
  invalid <- checked_names != x$name
  if (any(invalid)) {
    invalid_names <- x$name[invalid]
    plural <- if (sum(invalid) > 1) 's' else ''
    invalid_name_msg <- paste(invalid_names, collapse = ', ')
    error_msg <- glue('Variables definition contained invalid variable name{plural}: {invalid_name_msg}.\nVariable names must follow rules of R syntax.')
  }
  
  # Check that no reserved names are used
  used_reserved <- x$name %in% heRo_keywords
  if (any(used_reserved)) {
    reserved_index <- which(used_reserved)
    reserved_names <- x$name[reserved_index]
    plural <- if (sum(used_reserved) > 1) 's' else ''
    reserved_msg <- paste(reserved_names, collapse = ', ')
    error_msg <- glue('Variables definition contained invalid variable name{plural}: {reserved_msg}.\nName{plural} reserved for internal use.')
  }
  
  # Check that name does not start with a period
  period <- substr(x$name, 1, 1) == '.'
  if (any(period)) {
    period_index <- which(period)
    period_names <- x$name[period_index]
    plural <- if (sum(period) > 1) 's' else ''
    period_msg <- paste(period_names, collapse = ', ')
    error_msg <- glue('Variables definition contained invalid variable name{plural}: {period_msg}.\nNames starting with a period reserved for internal use.')
  }
  
  # Check that formulas are not blank
  blank_index <- which(any(x$formula == '' | is.na(x$formula)))
  if (length(blank_index) > 0) {
    plural <- if (length(blank_index) > 1) 's' else ''
    blank_names <- x$name[blank_index]
    blank_msg <- paste(blank_names, collapse = ', ')
    error_msg <- glue('Variables definition contained invalid variable name{plural}: {blank_msg}.')
  }
  
  if (error_msg != '') stop(error_msg, call. = F)
}

# Type coercion methods
as.variables <- function(x) {
  UseMethod('as.variables', x)
}
as.variables.variables <- function(x) {
  x
}
as.variables.data.frame <- function(x) {
  class(x) <- c('variables', class(x))
  x
}
