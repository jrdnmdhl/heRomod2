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
  lapply(sheet_names, function(x) as_tibble(readWorkbook(path, sheet = x)))
}

#' @export
read_model <- function(path) {
  model <- read_workbook(glue('{path}/model.xlsx'))
  model$tables <- list.files(glue('{path}/data')) %>%
    purrr::set_names(., gsub('.csv', '', ., fixed = T)) %>%
    purrr::map(~read.csv(glue('{path}/data/{.}'), stringsAsFactor = F, check.names = F))
  model$scripts <- list.files(glue('{path}/scripts')) %>%
    purrr::set_names(., gsub('.R', '', ., fixed = T)) %>%
    purrr::map(~readr::read_file(glue('{path}/scripts/', .)))
  class(model) <- "heRoModel"
  return(model)
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
  names[which(!(names %in% colnames(x)))]
}

is_in_segment <- function(segment, strat = NULL, grp = NULL, include_globals = T) {
  
  if (!is.null(strat)) {
    is_my_strat <- segment$strategy == strat & !is.na(strat)
    not_strat_spec <- (is.na(strat) | is.null(strat) | strat == '') & include_globals
  } else {
    is_my_strat <- F
    not_strat_spec <- include_globals
  }
  if (!is.null(grp)) {
    is_my_group <- segment$group == grp & !is.na(grp)
    not_group_spec <- (is.na(grp) | is.null(grp) | grp == '') & include_globals
  } else {
    is_my_group <- F
    not_group_spec <- include_globals
  }
  
  (is_my_strat | not_strat_spec) & (is_my_group | not_group_spec)
}

parse_csl <- function(string, flatten = T) {
  gsub('\\s', '', string) %>%
    strsplit('[,\\s]+', perl = T) %>%
    {if (flatten) flatten_chr(.) else .}
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

has_st_dependency <- function(x, extras = NULL) {
  any(x$depends %in% c(state_time_keywords, extras))
}


check_state_time <- function(vars, states, transitions, health_values, econ_values) {

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
    }) %>%
    left_join(select(states, name, max_state_time), by = c('state' = 'name')) %>%
    transmute(
      state,
      uses_st = ifelse(max_state_time > 1 && uses_st, TRUE, FALSE),
      max_st = ifelse(uses_st, max_state_time, 1)
    )
  
  st_df
}

parse_settings <- function(settings) {
  # Convert the settings table into a named
  # list.
  settings$value %>%
    as.list() %>%
    setNames(settings$setting)
}

#' Generate List of Names for Error Messages
#' 
#' Generates a comma-separated character of quoted names in order to list
#' items referenced in error messages.
#' 
#' @param x character vector of names
#' 
#' @return character with comma-separated list of quoted names
err_name_string <- function(x) {
  paste0('"', x, '"', collapse = ', ')
}

#' Check if Name is Valid
#' 
#' Checks character vector to see if elements follow the rules of heRomod2 variable names,
#' which must start with a letter and include only letters, numbers, and underscores.
#' 
#' @param x character vector of names to check
#' 
#' @return logical vector indicating whether the elements of `x` are valid.
is_valid_name <- function(x) grepl('^[[:alpha:]]+[[:alnum:]_]*$', x)

#' Check If Character is Empty/Missing
#' 
#' Check if a character is either `NA` or empty.
#' 
#' @param x character vector to be checked
#' 
#' @return logical vector indicating whether the elements of `x` are empty or missing.
is_faslsy_chr <- function(x) {
  is.na(x) | x == ''
}


#' Check Table
#' 
#' Check a model inputs dataframe based on a given specification dataframe. Specification
#' dataframes are used to check input dataframes, ensure that required columns are present
#' and impute missing values for non-required columns.
#' 
#' @param df The input dataframe to be checked
#' @param spec The specification dataframe
#' @param context A string used in error messages to indicate the type of input dataframe
#' 
#' @return The input dataframe with missing values imputed
check_tbl <- function(df, spec, context) {
  
  spec_cn <- spec$name
  n_col <- nrow(spec)
  n_row <- nrow(df)
  
  for (i in seq_len(n_col)) {
    col_name <- spec$name[i]
    required <- spec$required[i]
    type <- spec$type[i]
    default <- spec$default[i]
    fallback <- spec$fallback[i]
    values <- df[[spec_cn[i]]]
    
    # Handle case where the entire column is missing
    if (is.null(values)) {
      miss_data <- rep(TRUE, n_row)
    } else {
      miss_data <- is_faslsy_chr(values)
    }
    
    # Impute missing values
    if (any(miss_data)) {
      if (required) {
        # Throw error if a required column has missing values
        err_msg <- glue('{context} has missing values in required column: {col_name}.')
        stop(err_msg, call. = F)
      } else if (!is.na(fallback)) {
        # If a fallback is specified, replace missing values with fallback
        values[miss_data] <- df[[fallback]][miss_data]
      } else {
        # If a default is specified, replace missing values with default
        values[miss_data] <- default
      }
    }
    
    # Update data frame
    df[[col_name]] <- convert_to_type(values, type)
  }
  
  # Use only the columns defined in the spec
  select(df, {{spec_cn}})
}

convert_to_type <- function(x, type) {
  func <- eval(parse(text = paste0('as.', type)))
  func(x)
}
