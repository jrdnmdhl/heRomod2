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
  structure(obj, class = class)
}

is_in_segment <- function(d, strat, grp, include_globals = T) {
  is_my_strat <- d$strategy == strat
  is_my_group <- d$group == grp
  not_strat_spec <- (is.na(d$strategy) | d$strategy == '') & include_globals
  not_group_spec <- (is.na(d$group) | d$group == '') & include_globals
  
  (is_my_strat | not_strat_spec) & (is_my_group | not_group_spec)
}

read_in_tables <- function(tables, env, log) {
  if (length(tables) > 0) {
    log_print_section_break(log)
    log_print_heading('Reading in Tabular Data', level = 2, log = log)
  }
  for (name in names(tables)) {
    assign(name, tables[[name]], envir = env)
    log_print_heading(name, level = 3, log = log)
    log_print_table(dplyr::as_data_frame(tables[[name]]), log = log)
  }
}

read_in_trees <- function(trees, env, log) {
  if ((!is.null(trees)) && nrow(trees) > 0) {
    env$.trees <- trees
    log_print_heading('Reading in Trees', level = 3, log = log)
  }
}

run_scripts <- function(scripts, env, log) {
  if (length(scripts) > 0) {
    log_print_section_break(log)
    log_print_heading('Running R Scripts', level = 2, log = log)
  }
  for (name in names(scripts)) {
    log_print_heading(paste0('Running: ', name), level = 3, log = log)
    eval(parse(text = scripts[[name]]), envir = env)
  }
}

get_cycle_length_days <- function(settings) {
  days_per_cl_unit <- days_per_unit(settings$value[settings$setting == cl_unit_code][1])
  cl <- as.numeric(settings$value[settings$setting == cl_code][1])
  cl * days_per_cl_unit
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

get_n_cycles <- function(settings) {
  days_per_tf_unit <- days_per_unit(settings$value[settings$setting == tf_unit_code][1])
  days_per_cl_unit <- days_per_unit(settings$value[settings$setting == cl_unit_code][1])
  tf <- as.numeric(settings$value[settings$setting == tf_code][1])
  cl <- as.numeric(settings$value[settings$setting == cl_code][1])
  
  floor((tf * days_per_tf_unit) / (cl * days_per_cl_unit))
}

days_per_unit <- function(unit) {
  switch(
    unit,
    "Days" = 1,
    "Weeks" = 7,
    "Months" = 365/12,
    "Years" = 365
  )
}


#' Sort Variables List
#'
#' Sorts a heRovar_list object in order to solve issues of dependency
#' resolution and identify any circular references.
#'
#' @param x The heRovar_list object to be sorted
#' @param ... Unused parameters to match sort call signature
#'
#' @export
sort.heRovar_list <- function(x, ...) {
  
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
  while (length(unordered) > 0) {
    
    # Define a vector which will hold the indices of each
    # parameter to be moved to the ordered list
    to_remove <- c()
    
    # Loop through each unordered parameter
    for (i in seq_len(length(unordered))) {
      
      # If all the parameters its formula references
      if (all(unordered[[i]] %in% ordered)) {
        
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
    
    if (length(to_remove) == 0) {
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

#' @export
`[.heRovar_list`  <- function(x, i, ...) {
  as.heRovar_list(NextMethod())
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
  tibble::tibble(...)[[x]]
}

is.empty <- function(x) {
  is.na(x) | x == ''
}
