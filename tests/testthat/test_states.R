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
    'States definition contained duplicate names for states: "relapse_free_on_tx', fixed=TRUE
  )
})

test_that('missing state names are detected', {
  
  # Parse the variables specification
  expect_error(
    heRomod2:::parse_states(state_tests$miss_name, settings),
    'States definition contained invalid names for variables: "NA". Names must start with a letter and contain only letters, numbers, and underscores.', fixed=TRUE
  )
})

test_that('wrong state names are detected', {
  
  # Parse the variables specification
  expect_error(
    heRomod2:::parse_states(state_tests$wrong_name, settings),
    'States definition contained invalid names for variables: "1". Names must start with a letter and contain only letters, numbers, and underscores.', fixed=TRUE
  )
})

test_that('reserved name for state names are detected', {
  
  # Parse the variables specification
  expect_error(
    heRomod2:::parse_states(state_tests$reserved, settings),
    'States definition contained names reserved keyword: "cycle".', fixed=TRUE
  )
})

test_that('sum of initial probabilities is 1', {
  
  # Parse the variables specification
  expect_error(
    heRomod2:::parse_states(state_tests$probab, settings),
    'States sum of initial probablities is not 1 : 0.5.', fixed=TRUE
  )
})

test_that("state cylcle limit unit is calendar value", {
  
  # Parse the variables specification
  expect_error(
    heRomod2:::parse_states(state_tests$limit_unit, settings),
    'States wrong state cycle limit unit : xxx.', fixed=TRUE
  )
})
  
  test_that("state cylcle limit is non-negative number", {
    
    # Parse the variables specification
    expect_error(
      heRomod2:::parse_states(state_tests$limit_val, settings),
      'States wrong state cycle limit is not non-negative number: b, a, -2.', fixed=TRUE
    )  
})
  
  test_that("state share state", {
    
    # Parse the variables specification
    expect_error(
      heRomod2:::parse_states(state_tests$share, settings),
      'tates wrong state share time value : foo.', fixed=TRUE
    )  
  })
