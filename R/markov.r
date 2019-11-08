parse_markov <- function(model) {
  define_object_(model, class = 'markov')
}

run_segment <- function(segment, model, env, ...) {
  UseMethod('run_segment', model)
}

run_segment.markov <- function(segment, model, env, ...) {
  
  # Capture the extra arguments provided to function
  dots <- list(...)
  
  # Count the required number of model cycles and the
  # cycle length in days
  n_cycles <- get_n_cycles(model$settings)
  cycle_length_days <- get_cycle_length_days(model$settings)
  
  # Parse the specification tables provided for states,
  # variables, transitions, values, and summaries
  uneval_states <- parse_states(model$states, model$settings)
  uneval_vars <- parse_variables(model$variables, segment, trees = model$trees)
  uneval_trans <- parse_trans_markov(model$transitions, uneval_states, uneval_vars)
  uneval_health_values <- parse_values(model$health_values, uneval_vars)
  #uneval_econ_values <- parse_values(model$economic_values)
  
  # Check inside the variables, transitions, & values for
  # any state-time dependency since this will inform the
  # creation of tunnel states
  state_time_use <- check_state_time(
    uneval_vars,
    uneval_trans,
    uneval_health_values,
    uneval_health_values
  )

  # Create a "namespace" which will contain evaluated
  # variables so that they can be referenced.
  ns <- create_namespace(model, segment)
  
  # Evaluate variables, initial state probabilities, transitions,
  # values, & summaries.
  eval_vars <- eval_variables(uneval_vars, ns)
  eval_states <- eval_states(uneval_states, eval_vars)
  eval_trans <- eval_trans_markov_lf(uneval_trans, eval_vars, model$settings$reduce_state_cycle)
  eval_health_values <- evaluate_values(uneval_health_values, eval_vars, model$settings$reduce_state_cycle)
  
  # Determine the number of tunnel states that need to be created
  # for each state.
  st_maxes <- rbind(
    rename(eval_trans[ , c('from', 'max_st')], state = from),
    eval_health_values[ , c('state', 'max_st')]
  ) %>%
    group_by(state) %>%
    summarize(max_st = max(max_st))
  
  # Remove any calculated values for transitions & values that are
  # not needed.
  eval_trans_limited <- select(eval_trans, -max_st) %>%
    left_join(st_maxes, by = c('from' = 'state')) %>%
    filter(state_cycle <= max_st)
  eval_health_values_limited <- select(eval_health_values, -max_st) %>%
    left_join(st_maxes, by = c('state' = 'state')) %>%
    filter(state_cycle <= max_st)
  
  # Transform transitions and values to matrices.
  eval_trans_mat <- lf_to_tmat(eval_trans_limited)
  expanded_state_names <- dimnames(eval_trans_mat)[[2]]
  eval_health_values_mat <- values_to_vmat(eval_health_values_limited, expanded_state_names)
  
  # Calculate Trace Probabilities
  expand_init <- expand_init_states(eval_states, st_maxes)
  trace <- markov_trace(expand_init, eval_trans_mat)
  
  # Create the object to return that will summarize the results of
  # this segment.
  segment$uneval_vars <- list(uneval_vars)
  segment$eval_vars <- list(eval_vars)
  segment$eval_trans <- list(eval_trans_limited)
  segment$eval_values <- list(eval_health_values_limited)
  segment$inital_state <- list(eval_states)
  segment$tmat <- list(eval_trans_mat)
  segment$vmat <- list(eval_health_values_mat)
  segment$trace <- list(trace)
  segment
}

markov_trace <- function(init, matrix) {
  print(init)
}
