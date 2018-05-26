context("Parameters")
library(dplyr)

test_that("Defining an evaluating parameters", {
  params <- define_parameters(
    x = 1000,
    y = 34.4,
    z = x * y,
    a = cars,
    b = lm(speed~dist, data = a),
    c = predict(b, newdata = data.frame(dist = y))
  )
  params_ns <- heRomod2:::define_namespace(mtcars)
  params_eval <- heRomod2::evaluate(params, params_ns)
  expect_true(all(params_eval$df$x == 1000))
  expect_true(all(params_eval$df$y == 34.4))
  expect_true(all(params_eval$df$z == 34400))
  expect_equal(params_eval$df$c, rep(13.97943, nrow(mtcars)), tolerance = 1e-07)
  expect_true(class(params_eval$env$a) == "data.frame")
  expect_true(class(params_eval$env$b) == "lm")
})

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

  # Missing column 'formula'
  col1_df <- readxl::read_xlsx(
    system.file("test_cases", "parameters_tests.xlsx", package = "heRomod2"),
    "colname1"
  )
  expect_error(
    heRomod2:::define_parameters_tabular(col1_df),
    "Parameters table must have columns 'name' and 'formula'"
  )

  # Missing column 'name'
  col2_df <- readxl::read_xlsx(
    system.file("test_cases", "parameters_tests.xlsx", package = "heRomod2"),
    "colname2"
  )
  expect_error(
    heRomod2:::define_parameters_tabular(col2_df),
    "Parameters table must have columns 'name' and 'formula'"
  )

  # Invalid R-Expression
  invalid_df <- readxl::read_xlsx(
    system.file("test_cases", "parameters_tests.xlsx", package = "heRomod2"),
    "invalid"
  )
  expect_error(
    heRomod2:::define_parameters_tabular(invalid_df),
    "Parameter 'a' is not a valid R-Expression"
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