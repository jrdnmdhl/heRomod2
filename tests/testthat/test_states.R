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
settings <- parse_settings(state_tests$setting)


test_that('states are  properly load', {
  expect_success(res <- heRomod2:::parse_states(state_tests$states, settings))
})

test_that('duplicate state names are detected', {
  
  # Parse the variables specification
  expect_error(
    heRomod2:::parse_states(state_tests$dup_names, settings),
    'factor level [2] is duplicated'
  )
})
