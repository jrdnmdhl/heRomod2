context("Parameters")
library(dplyr)

test_that("Defining parameters from tabular specification", {

  # Define and evaluate pre-sorted parameters object
  sorted_df <- readxl::read_xlsx(
    system.file("test_cases", "parameters_tests.xlsx", package = "heRomod2"),
    "sorted"
  )
  sorted_params <- heRomod2:::define_parameters_tabular(sorted_df)
  sorted_ns <- heRomod2:::define_namespace(mtcars)
  sorted_params_eval <- heRomod2::evaluate(sorted_params, sorted_ns)

  # Define and evaluate unsorted parameters object
  unsorted_df <- readxl::read_xlsx(
    system.file("test_cases", "parameters_tests.xlsx", package = "heRomod2"),
    "unsorted"
  )
  unsorted_params <- heRomod2:::define_parameters_tabular(unsorted_df)

  # Detect circular references
  circular_df <- readxl::read_xlsx(
    system.file("test_cases", "parameters_tests.xlsx", package = "heRomod2"),
    "circular"
  )
  expect_error(
    heRomod2:::define_parameters_tabular(circular_df),
    "Circular reference in parameters"
  )
})
