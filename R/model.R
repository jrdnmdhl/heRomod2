#' Run a Model
#'
#' Takes a model specification object and runs the model.
#'
#' @param model A heRo_model object.
#' @param ... additional arguments.
#' 
#' @return A list containing the results of the model.
#' 
#' @export
run_model <- function(model, ...) {
  
  # Capture the extra arguments
  dots <- list(...)
  
  # Create a results object
  res <- list()
  
  # Parse the model
  parsed_model <- parse_model(model, ...)
  
  # Get model segments
  if (is.null(dots$newdata)) segments <- get_segments(parsed_model)
  else segments <- dots$newdata
  
  # Split by segment, evaluate each segment in parallel, combine results
  res$segments <- segments %>%
    rowwise() %>%
    group_split() %>%
    map(function(segment) run_segment(segment, parsed_model, ...)) %>%
    bind_rows()
  
  # Process the results
  
  # Return
  res
}

#' @export
run_model1 <- function(model, ...) {
  
  # Capture the extra arguments
  dots <- list(...)
  
  # Create a results object
  res <- list()
  
  # Parse the model
  parsed_model <- parse_model(model, ...)
  
  # Get model segments
  if (is.null(dots$newdata)) segments <- get_segments(parsed_model)
  else segments <- dots$newdata
  
  # Split by segment, evaluate each segment in parallel, combine results
  
  segments1 <- segments %>%
    rowwise() %>%
    group_split() 
  #res$segments <- map(segments1, function(segment) run_segment(segment, parsed_model, ...)) %>%
  #  bind_rows()
  
  rrr <- run_segment(segments1[[1]], parsed_model, ...)
  
  # Process the results
  
  # Return
  #res
}

parse_model <- function(model, ...) {
  
  # Create a new environment from the calling environment which will be used
  # to store model variables.
  model$env <- new.env(parent = parent.frame())
  
  # Load tables & trees into the environment
  load_tables(model$tables, model$env)
  load_trees(model$trees, model$env)
  
  # Run any model scripts within that environment
  run_scripts(model$scripts, model$env)
  
  # If no groups are defined, create one group representing entire population
  if (nrow(model$groups) == 0) {
    model$groups <- create_default_group()
  }

  # Reformat settings as a list
  model$settings <- parse_settings(model$settings)

  # Set the class of the object based on model type
  class(model) <- c(model$settings$model_type, 'list')

  model
}
