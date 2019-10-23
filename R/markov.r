parse_markov <- function(model) {
  define_object_(model, class = 'markov')
}

run_segment <- function(segment, model, env, ...) {
  UseMethod('run_segment', model)
}

run_segment.markov <- function(segment, model, env, ...) {
  
  # Capture the extra arguments
  dots <- list(...)
  
  # Count the required number of cycles
  n_cycles <- get_n_cycles(model$settings)
  
  # Count the required number of cycles
  cycle_length_days <- get_cycle_length_days(model$settings)
  
  # Create the states object
  uneval_states <- parse_states(model$states, model$settings)
  
  # Create and sort the parameters object
  uneval_vars <- parse_variables(model$variables, segment, trees = model$trees)
  
  # Create the transitions object
  uneval_trans <- parse_trans_markov_lf(model$transitions, uneval_states, uneval_vars)
  
  # Create the values objects
  uneval_health_values <- parse_values(model$health_values, uneval_vars)
  #uneval_econ_values <- parse_values(model$economic_values)
  
  # Check for state-time dependency
  state_time_use <- check_state_time(uneval_vars, uneval_trans, uneval_health_values, uneval_health_values)
  
  # Create time and cl vars
  time_vars <- time_variables(model$settings, model$states)
  cl_vars <- cl_variables(model$settings)
  
  # Create the starting namespace object
  ns <- define_namespace(model$env, time_vars, cl_vars) %>%
    update_segment_ns(segment)
  
  # Evaluate the variables
  eval_vars <- eval_variables(uneval_vars, ns)
  
  # Evaluate transitions to longform table
  trans <- eval_trans_markov_lf(uneval_trans, eval_vars, model$settings$reduce_state_cycle)
  
  # Evaluate values to longform table
  eval_health_values <- evaluate_values(uneval_health_values, eval_vars, model$settings$reduce_state_cycle)
  
  # Determine extent of state time in each state
  st_maxes <- rbind(
    rename(trans[ , c('from', 'max_st')], state = from),
    eval_health_values[ , c('state', 'max_st')]
  ) %>%
    group_by(state) %>%
    summarize(max_st = max(max_st))
  
  # Reduce extra state time 
  trans <- select(trans, -max_st) %>%
    left_join(st_maxes, by = c('from' = 'state')) %>%
    filter(state_cycle <= max_st)
  eval_health_values <- select(eval_health_values, -max_st) %>%
    left_join(st_maxes, by = c('state' = 'state')) %>%
    filter(state_cycle <= max_st)
  
  # Transform transitions to matrix
  mat <- lf_to_matrix(trans)
  
  # Return segment row
  segment$uneval_vars <- list(uneval_vars)
  segment$eval_vars <- list(eval_vars)
  segment$eval_trans <- list(trans)
  segment$eval_values <- list(eval_health_values)
  segment$mat <- list(mat)
  segment
}
