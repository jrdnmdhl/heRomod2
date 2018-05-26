context("Parameters")
library(dplyr)

test_that("Defining and evaluating parameters from tabular specification", {

  # Sorted and unsorted parameters object work and yield same result
  sorted_df <- readxl::read_xlsx(
    system.file("test_cases", "parameters_tests.xlsx", package = "heRomod2"),
    "sorted"
  )
  sorted_params <- heRomod2:::define_parameters_tabular(sorted_df)
  sorted_ns <- heRomod2:::define_namespace(mtcars)
  sorted_params_eval <- heRomod2::evaluate(sorted_params, sorted_ns)
  unsorted_df <- readxl::read_xlsx(
    system.file("test_cases", "parameters_tests.xlsx", package = "heRomod2"),
    "unsorted"
  )
  unsorted_params <- heRomod2:::define_parameters_tabular(unsorted_df)
  unsorted_ns <- heRomod2:::define_namespace(mtcars)
  unsorted_params_eval <- heRomod2::evaluate(unsorted_params, unsorted_ns)
  expect_equal(
    sorted_params_eval$df,
    unsorted_params_eval$df[colnames(sorted_params_eval$df)]
  )
  expect_equal(
    as.list(sorted_params_eval$env),
    as.list(unsorted_params_eval$env)[names(as.list(sorted_params_eval$env))]
  )

  # Invalid R-Expression
  invalid_df <- readxl::read_xlsx(
    system.file("test_cases", "parameters_tests.xlsx", package = "heRomod2"),
    "invalid"
  )
  expect_error(
    heRomod2:::define_parameters_tabular(invalid_df),
    "Parameter 'g' is not a valid R-Expression"
  )

  # Error in parameter evaluation
  error_df <- readxl::read_xlsx(
    system.file("test_cases", "parameters_tests.xlsx", package = "heRomod2"),
    "error"
  )
  error_params <- heRomod2:::define_parameters_tabular(error_df)
  error_ns <- heRomod2:::define_namespace(mtcars)
  expect_error(
    heRomod2::evaluate(error_params, error_ns),
    "Error in parameter 'a', object 'z' not found"
  )

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
