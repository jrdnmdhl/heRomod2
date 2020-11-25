# Check Transitions
# ================
context("Transitions")
#library(tibble)
#library(dplyr)
#library(heRomod2)

# Prepare data for test cases
transition_tests <- system.file("test_cases", "test_transitions.xlsx", package = "heRomod2") %>%
  read_workbook() 
state_names <- transition_tests$state$name

test_that('Check column names: from, to, formula', {
  
  # Parse the variables specification
  expect_error(
    heRomod2:::check_trans_markov(transition_tests$transition_col_wrong, state_names) ,
    "Transitions definition was missing column: to."
  )
})


test_that('Check missing states', {
  
  # Parse the variables specification
  expect_error(
    heRomod2:::check_trans_markov(transition_tests$missing_state, state_names) ,
    "Transitions definition missing state: dead."
  )
})


test_that('Check that no transitions are duplicated', {
  
  # Parse the variables specification
  expect_error(
    heRomod2:::check_trans_markov(transition_tests$dupe, state_names) ,
    "Transitions definition contains duplicate enties for transition: relapse_free_off_tx→relapse."
  )
})


test_that('Check missing formulas', {
  
  # Parse the variables specification
  expect_error(
    heRomod2:::check_trans_markov(transition_tests$blank_formula, state_names) ,
    "Transitions definition contained blank formula for transitions: relapse_free_on_tx→relapse_free_on_tx."
  )
})


test_that('Check formulas', {
  
  segment <- tibble::tibble(strategy = "S1", group = "G1")
  test_ns <- heRomod2:::create_test_ns(segment)
  cl_unit_code <- 'cycle_length_unit'
  transition_tests$settings <- heRomod2:::parse_settings(transition_tests$settings)
  n_cycles <- heRomod2:::get_n_cycles(transition_tests$settings)
  cycle_length_days <- heRomod2:::get_cycle_length_days(transition_tests$settings)
  t_states <- heRomod2:::parse_states(transition_tests$state, transition_tests$settings)
  t_vars <- heRomod2:::parse_seg_variables(transition_tests$variables, segment, trees = transition_tests$trees)
  t_trans <- heRomod2:::parse_trans_markov(transition_tests$wrong_formula, t_states, t_vars)
  t_trans2 <- heRomod2:::parse_trans_markov(transition_tests$wrong_formula2, t_states, t_vars)
  
  # Parse the variables specification
  expect_error(
    heRomod2:::eval_trans_markov_lf(t_trans, test_ns,  simplify = transition_tests$settings$reduce_state_cycle),
    "Transitions: Error in felse(state_cycle > 3, 0.2, 0.1): could not find function \"felse\"\n Formula: felse(state_cycle > 3, 0.2, 0.1).", fixed=TRUE
  )
  
  expect_error(
    heRomod2:::eval_trans_markov_lf(t_trans2, test_ns,  simplify = transition_tests$settings$reduce_state_cycle),
    "Transitions: Error in formula syntax.\n Formula: 0.0a.", fixed=TRUE
  )
})


