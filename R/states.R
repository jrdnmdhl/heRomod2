


parse_states <- function(states, settings) {
  
  # Check that variables definition is valid
  check_states(states)
  
  # If state time limit is unspecified, assume infinite
  states$state_cycle_limit <- ifelse(
    is.na(states$state_cycle_limit),
    Inf,
    states$state_cycle_limit
  )
  
  # If state time limit unit is undefined, use cycles
  states$state_cycle_limit_unit <- ifelse(
    is.na(states$state_cycle_limit_unit),
    'cycles',
    states$state_cycle_limit_unit
  )
  
  # Parse initial probability formulas, calculate maximum tunnel states
  parsed_states <- states %>%
    mutate(
      formula = map(initial_probability, as.heRoFormula),
      max_state_time = ceiling(
        days_per_unit(state_cycle_limit_unit, settings) * state_cycle_limit / get_cycle_length_days(settings)
      ),
      state_group = ifelse(is.na(state_group), paste0('.', name), state_group),
      share_state_time = ifelse(is.na(share_state_time), F, share_state_time)
    ) %>%
    select(
      name,
      display_name,
      description,
      formula,
      state_group,
      share_state_time,
      max_state_time
    ) %>%
    sort_variables()
  
  # Construct Object & Return
  as.states(parsed_states)
}

check_states <- function(x,context = "States") {
    
    error_msg = ''
    
    # Check that there are no duplicate names
    dupe <- duplicated(x$name)
    if (any(dupe)) {
      dupe_names <- unique(x$name[dupe])
      dupe_msg <- err_name_string(dupe_names)
      error_msg <- paste0(context, ' definition contained duplicate names for states: ', dupe_msg, '.')
    }
    
    # Check that state names are valid
    invalid <- !is_valid_name(x$name)
    if (any(invalid)) {
      invalid_names <- x$name[invalid]
      invalid_name_msg <- err_name_string(invalid_names)
      error_msg <- paste0(
        context,
        ' definition contained invalid names for variables: ',
        invalid_name_msg,
        '. Names must start with a letter and contain only letters, numbers, and underscores.'
      )
    }
    
    # Check that no reserved names are used
    used_reserved <- x$name %in% heRo_keywords
    if (any(used_reserved)) {
      reserved_index <- which(used_reserved)
      reserved_names <- x$name[reserved_index]
      reserved_msg <- err_name_string(reserved_names)
      error_msg <- paste0(
        context,
        ' definition contained names reserved keyword: ',
        reserved_msg,
        '.'
      )
    }
    
    # Check that sum of initial probabilities is 1 
    sum_prob <- sum(x$initial_probability)
    if (sum_prob != 1) {
      reserved_msg = sum_prob
      error_msg <- paste0(
        context,
        ' sum of initial probablities is not 1 : ',
        reserved_msg,
        '.'
      )
    }    
    if (error_msg != '') stop(error_msg, call. = F)
}

eval_states <- function(x, ns) {
  # Limit variables to first cycle
  cloned_ns <- clone_namespace(ns)
  cloned_ns$df <- cloned_ns$df[1, ]
  eval_variables(x, cloned_ns, T)$df[ ,x$name]
}

expand_init_states <- function(x, expand) {
  n_states_exp <- sum(expand$max_st)
  init_mat <- matrix(numeric(n_states_exp), nrow = 1)
  col_names <- character(n_states_exp)
  index <- 1
  for (i in colnames(x)) {
    row <- expand[expand$state == i, ]
    indices <- seq(from = index, to = index + row$max_st[1] - 1)
    init_mat[1, index] <- x[[i]][1]
    col_names[indices] <- expand_state_name(
      rep(row$state[1], row$max_st[1]),
      seq(row$max_st[1])
    )
    index <- max(indices) + 1
  }
  colnames(init_mat) <- col_names
  init_mat
}

as.states <- function(x) {
  UseMethod('as.states', x)
}
as.states.states <- function(x) {
  x
}
as.states.data.frame <- function(x) {
  class(x) <- c('states', class(x))
  x
}

expand_state_name <- function(name, index) {
  paste0(name, '.', index)
}
