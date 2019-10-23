#' @export
read_model <- function(path) {
  model <- read_workbook(file.path(path, 'model.xlsx'))
  model$tables <- list.files(file.path(path, 'data')) %>%
    purrr::set_names(., gsub('.csv$', '', .)) %>%
    purrr::map(~read.csv(file.path(path, 'data', .), stringsAsFactor = F, check.names = F))
  model$scripts <- list.files(file.path(path, 'scripts')) %>%
    purrr::set_names(., gsub('.R$', '', ., fixed = T)) %>%
    purrr::map(~readr::read_file(file.path(path,'scripts', .)))
  
  define_object_(model, 'heRomodel')
}


#' Read an Excel Workbook
#' 
#' Takes the path of an excel workbook and reads it in as a named list of
#' data frames.
#' 
#' @param path the path to the workbook.
#' 
#' @return a named list of data.frames
#' 
#' @export
read_workbook <- function(path) {
  sheet_names <- getSheetNames(path)
  names(sheet_names) <- sheet_names
  lapply(sheet_names, function(x) readWorkbook(path, sheet = x))
}

define_object <- function(..., class) {
  define_object_(list(...), class)
}

define_object_ <- function(obj, class) {
  class(obj) <- class
  obj
}

create_default_group <- function() {
  tibble(
    name = 'all',
    display_name = 'All Patients',
    description = 'Entire model population.',
    weight = 1,
    enabled = 1
  )
}

load_tables <- function(tables, env) {
  for (name in names(tables)) {
    assign(name, tables[[name]], envir = env)
  }
}

load_trees <- function(trees, env) {
  if ((!is.null(trees)) && nrow(trees) > 0) {
    env$.trees <- trees
  }
}

run_scripts <- function(scripts, env) {
  for (name in names(scripts)) {
    eval(parse(text = scripts[[name]]), envir = env)
  }
}


get_segments <- function(model) {
  
  if (nrow(model$groups) == 0) {
    model$groups <- tibble::tibble(
      name = 'all',
      display_name = 'All Patients',
      description = 'All Patients',
      weight = 1,
      enabled = 1
    )
  }
  
  expand.grid(
    group = model$groups$name,
    strategy =  model$strategies$name,
    stringsAsFactors = F
  )
}

check_missing_colnames <- function(x, names) {
  colnames(x)[which(!(names %in% colnames(x)))]
}



#' Sort Variables List
#'
#' Sorts a heRovar_list object in order to solve issues of dependency
#' resolution and identify any circular references.
#'
#' @param x The heRovar_list object to be sorted
#' @param ... Unused variables to match sort call signature
#'
#' @export
sort.heRovar_list <- function(x, ...) {
  
  # Extract variable names
  par_names <- names(x)
  
  # Extract the names referenced in each variable's
  # formula
  var_list <- purrr::map(x, function(y) {
    vars <- c(y$depends, y$after)
    vars[vars %in% par_names]
  })
  
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
        
        # Extract any decision tree probability calls
        p_calls <- extract_func_calls(x[[current_var]]$lazy$expr, 'p')
        tree_deps <- map(p_calls, function(y) {
          referenced_nodes <- x[[y$arg2]]$node_depends %>%
            keep(~any(.$tags %in% y$arg1)) %>%
            map(~.$depends) %>%
            flatten_chr()
        }) %>% flatten_chr()
        
        # Assemble first-order depencies
        fo_deps <- unique(c(x[[current_var]]$depends, tree_deps))
        
        # Add second+ order dependencies to variable
        x[[current_var]]$depends <- x %>%
          .[fo_deps] %>%
          lapply(function(x) x$depends) %>%
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
  
  # Sort the variables list according to the new order
  res <- x[ordered]
  res
  
}

#' @export
`[.heRovar_list`  <- function(x, i, ...) {
  as.heRovar_list(NextMethod())
}

parse_csl <- function(string) {
  gsub('\\s', '', string) %>%
    strsplit(',') %>%
    flatten_chr()
}


#' Vectorized Switch Statement
#' 
#' @param x The condition statement
#' @param ... name-value pairs where name repesents cases of condition
#' statement and values represent the value it will take on in each case.
#' 
#' @return Returns value for the given case.
#' 
#' @export
vswitch <- function(x, ...) {
  args <- list(...)
  opts <- names(args)
  xMat <- matrix(rep(x, length(args)), nrow = length(x), ncol = length(args), byrow = T)
  optsMat <- matrix(rep(opts, length(x)), nrow = length(x), ncol = length(args), byrow = T)
  mat <- as.matrix(do.call(tibble, args))
  if (nrow(mat) == 1) {
    mat <- mat[rep(1, nrow(xMat)), ]
  }
  unname(mat[which(xMat == optsMat)])
}

is.empty <- function(x) {
  is.na(x) | x == ''
}

`%&%` <- function(a,b) { paste0(a,b)}

extract_call_vars <- function(expr) {
  call_vars <- lapply(expr, function(x) all.vars(x))
  names(call_vars) <- c('func', paste0('arg', seq_len(length(expr) - 1)))
  call_vars
}

extract_func_calls <- function(expr, funcs) {
  if (is.call(expr)) {
    if (as.character(expr[[1]]) %in% funcs) {
      ret <- list(extract_call_vars(expr))
    } else {
      ret <- unlist(
        lapply(expr, function(x) extract_func_calls(x, funcs)),
        recursive = F
      )
    }
  } else if (is.name(expr) || is.atomic(expr)) {
    ret <- list()
  } else {
    ret <- unlist(
      lapply(expr, function(x) extract_func_calls(x, funcs)),
      recursive = F
    )
  }
  
  ret
}

resolve_tree_references <- function(calls) {
  unique(
      flatten_chr(
      map(calls, function(x) {
        '.trees.' %&% x$arg2[[1]] %&% '.' %&% x$arg1
      })
    )
  )
}

has_st_dependency <- function(x, extras = NULL) {
  any(x$depends %in% c(state_time_keywords, extras))
}


check_state_time <- function(vars, transitions, health_values, econ_values) {
  # Identify which vars have references to state time
  st_vars <- vars$name[map_lgl(vars$formula, ~has_st_dependency(.))]
  
  # Combine values & transitions, group by state, and identify
  # references to state time or variables referencing state time
  st_df <- select(transitions, from, formula) %>%
    rename(state = from) %>%
    rbind(
      select(health_values, state, formula),
      select(econ_values, state, formula)
    ) %>%
    group_by(state) %>%
    do({
      tibble(
        state = .$state[1],
        uses_st = any(map_lgl(.$formula, ~has_st_dependency(., extras = st_vars)))
      )
    })
  
  st_df
}
