context("States")
library(tibble)
library(dplyr)
library(heRomod2)

# Prepare data for test cases
state_tests <- system.file("test_cases", "test_states.xlsx", package = "heRomod2") %>%
  read_workbook() 

segment <- tibble::tibble(strategy = "S1", group = "G1")
test_ns <- heRomod2:::create_test_ns(segment)
cl_unit_code <- 'cycle_length_unit'
settings <- heRomod2:::parse_settings(state_tests$setting)


test_that('states are  properly load', {
  expect_error(
    res <- heRomod2:::parse_states(state_tests$states, settings), 
    NA)
})

test_that('duplicate state names are detected', {
  
  # Parse the variables specification
  expect_error(
    heRomod2:::parse_states(state_tests$dup_names, settings),
    'factor level [2] is duplicated', fixed=TRUE
  )
})



test_that('missing state names are detected', {
  
  # Parse the variables specification
  expect_error(
    heRomod2:::parse_states(state_tests$miss_name, settings),
    'factor level [2] is duplicated', fixed=TRUE
  )
})


test_that('wrong state names are detected', {
  
  # Parse the variables specification
  expect_error(
    heRomod2:::parse_states(state_tests$wrong_name, settings),
    'factor level [2] is duplicated', fixed=TRUE
  )
})
