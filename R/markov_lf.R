# Markov
#
# Markov models specified using longform table structure for
# transition matrices.
# --------------------------------------------------

# Parse a Markov Transitions Specification
#
# Takes a longform transitions specifications table, a states
# specification, and a variables specification and returns
# an unevalted transitions object.
parse_trans_markov <- function(x, states, vars) {
  
  # Extract the state names
  state_names <- states$name
  
  # Check transitions definition
  check_trans_markov(x, state_names)
  
  # Construct the transitions object
  x$formula <- map(x$formula, as.heRoFormula)
  x$name <- paste0(x$from, '→', x$to)
  res <- sort_variables(x, vars) %>%
    select(name, from, to, formula) %>%
    left_join(
      transmute(
        states, name = name,
        from_state_group = state_group
      ),
      by = c('from' = 'name')
    ) %>%
    left_join(
      transmute(
        states,
        name = name,
        to_state_group = state_group,
        share_state_time = share_state_time
      ),
      by = c('to' = 'name')
    )
  
  # Return result
  as.lf_markov_trans(res)
}

# Check a Markov Transitions Specification
#
# Takes a Markov transitions specifications table and a
# vector of state names and checks that the specification is
# valid.
check_trans_markov <- function(x, state_names) {
  
  error_msg <- ''
  
  # Check column names
  missing_cols <- check_missing_colnames(x, trans_markov_cols)
  if (length(missing_cols) > 0) {
    plural <- if (length(missing_cols) > 1) 's' else ''
    missing_msg <- paste(missing_cols, collapse = ', ')
    error_msg <- glue('Transitions definition was missing column{plural}: {missing_msg}.')
  }
  
  # Check that all from states are represented
  missing_states <- which(!(state_names %in% x$from))
  if (length(missing_states) > 0) {
    missing_state_names <- state_names[missing_states]
    plural <- if (length(missing_state_names) > 1) 's' else ''
    missing_state_msg <- paste(missing_state_names, collapse = ', ')
    error_msg <- glue('Transitions definition missing state{plural}: {missing_state_msg}.')
  }
  
  # Check that no transitions are duplicated
  trans_names <- paste0(x$from, '→', x$to)
  dupe <- duplicated(trans_names)
  if (any(dupe)) {
    dupe_names <- unique(trans_names[dupe])
    plural <- if (length(dupe_names) > 1) 's' else ''
    dupe_msg <- paste(dupe_names, collapse = ', ')
    error_msg <- glue('Transitions definition contains duplicate enties for transition{plural}: {dupe_msg}.')
  }
  
  # Check that formulas are not blank
  blank_index <- which(any(x$formula == '' | is.na(x$formula)))
  if (length(blank_index) > 0) {
    plural <- if (length(blank_index) > 1) 's' else ''
    blank_names <- paste0(x$from[blank_index], '→', x$to[blank_index])
    blank_msg <- paste(blank_names, collapse = ', ')
    error_msg <- glue('Transitions definition contained blank formula for transitions{plural}: {blank_msg}.')
  }
  
  if (error_msg != '') stop(error_msg, call. = F)
}



