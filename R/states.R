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
  parsed_states <- select(
      states,
      name,
      display_name,
      description,
      initial_probability,
      state_group,
      share_state_time,
      state_cycle_limit,
      state_cycle_limit_unit
    ) %>%
    mutate(
      initial_probability = map(states$initial_probability, as.heRoFormula),
      max_state_time = ceiling(
        days_per_unit(state_cycle_limit_unit, settings) * state_cycle_limit / get_cycle_length_days(settings)
      ),
      state_group = ifelse(is.na(state_group), paste0('.', name), state_group),
      share_state_time = ifelse(is.na(share_state_time), F, share_state_time)
    )
  
  # Construct Object & Return
  as.states(parsed_states)
}

check_states <- function(x) {

}

eval_states <- function(x, ns) {
  
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
