#' Evaluate a Model or Expression
#'
#' Evaluates a model or part of a model
#'
#' @param x The object to be evaluated
#' @param ... Additional arguments to be handled by method
#'
#' @rdname evaluate
#' @export
evaluate <- function(x, ...) {
  UseMethod("evaluate", x)
}

#' Evaluates a variable
#'
#' Takes a heRovar object and converts it to its character representation.
#'
#' @param x A heRovar object.
#' @return An atomic character vector representing the R-expression.
#' 
#' @export
evaluate.heRovar <- function(x, ns, ...) {
  
  # Attempt to evaluate expression
  x$env <- ns$env
  res <- tryCatch(
    {
      x$lazy$env <- ns$env
      lazy_eval(x$lazy, data = ns$df)
    },
    error = function(e) e,
    silent = T
  )
  
  # If an error occurs, send error message
  if ('error' %in% class(res)) {
    res <- error_codes$generic
  }
  
  # Return the result
  res
}

#' @export
evaluate.parameters <- function(x, ns, ...) {
  
  # Sort parameters
  x <- sort(x)
  
  # Iterate over each parameter and its name
  walk2(names(x), x, function(name, value) {
    res <- evaluate(value, ns)
    
    # Determine whether result is a vector or object parameter
    vector_type <- is.vector(res) && !is.list(res)
    if (vector_type && (length(res) == nrow(ns$df))) {
      # If a vector parameter, assign to data frame
      ns$df[name] <<- res
    } else {
      # If an object parameter, assign to environment
      assign(name, res, envir = ns$env)
    }
    # if (any(res %in% error_codes)) {
    #   cat(crayon::red(stringr::str_pad(name, 20, 'right') %+%  ': ' %+% '\U274C\n'))
    # } else {
    #   cat(crayon::red(stringr::str_pad(name, 20, 'right') %+%  ': ' %+% '\U2705\n'))
    # }
  })
  
  return(ns)
}

#' @export
evaluate.heRoModel <- function(model, ...) {
  
  # Capture the extra arguments
  dots <- list(...)
  
  print(eval(match.call()[[1]]))
  
  # Create a results object
  res <- list()
  
  # Define mode in which segments will be processed
  set_parallel_mode(dots$parallel_mode)
  
  # Determine the log level at which to evaluate model
  log <- get_log_level(dots$log)
  log_print_heading('Running Model', level = 2, log = log)
  
  # Create a new environment from the calling environment which will be used
  # to store model variables.
  model_env <- new.env(parent = parent.frame())
  
  # Read the tables into the environment
  read_in_tables(model$tables, model_env, log = log)
  
  # Run any model scripts within that environment
  run_scripts(model$scripts, model_env, log = log)
  
  # If no groups are defined, create one group representing entire population
  groups <- model$groups
  if (nrow(groups)) {
    groups <- dplyr::data_frame(
      name = 'all',
      display_name = 'All Patients',
      description = 'Entire model population.',
      weight = 1,
      enabled = 1
    )
  }
  
  # Define the segments for which model will be run
  if (is.null(dots$newdata)) dots$newdata <- tibble::tibble(simulation = 1)
  
  # Get model segments
  if (is.null(dots$newdata)) segments <- get_segments(model)
  else segments <- dots$newdata
  
  # Run model for each segment
  res$segments <- segments %>%
    dplyr::rowwise() %>%
    dplyr::group_split() %>%
    furrr::future_map(function(segment) evaluate_segment(segment, model, env = model_env, ...)) %>%
    dplyr::rbind_list()
  
  res
}

evaluate_segment <- function (segment, model, env, ...) {
  
  # Capture the extra arguments
  dots <- list(...)
  
  # Log that we are evaluating the strategy
  log_print_heading(
    paste0('Evaluating strategy: ', segment$strategy, ', group: ', segment$group),
    level = 2,
    log = dots$log
  )
  
  # Count the required number of cycles
  n_cycles <- get_n_cycles(model$settings)
  
  # Count the required number of cycles
  cycle_length_days <- get_cycle_length_days(model$settings)
  
  # Create the starting namespace object
  ns <- create_namespace(n_cycles, 1, cycle_length_days, env) %>%
    update_segment_ns(segment)
  
  # Create and sort the parameters object
  uneval_vars <- model$variables %>%
    dplyr::filter(is_in_segment(., segment$strategy, segment$group), !name %in% names(segment)) %>%
    define_parameters() %>%
    sort()
  
  # Evaluate the Parameters
  eval_vars <- evaluate(params, ns)
  
  # Return segment row
  segment$uneval_vars <- list(uneval_vars)
  segment$eval_vars <- list(eval_vars)
  segment
}
