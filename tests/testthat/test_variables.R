context("Variables")
library(tibble)

# Load test case data
var_tests <- system.file(
    "test_cases",
    "test_params.xlsx",
    package="heRomod2"
  ) %>%
  read_workbook()

segment <- tibble(strategy = "S1", group = "G1")
test_ns <- heRomod2:::create_test_ns(segment)


test_that('formula evaluation errors are handled properly', {
  
  # Parse the variables specification
  parsed_vars <- heRomod2:::parse_variables(var_tests$err, segment)
  
  # Check that a warning is produced naming all the parameters generating
  # errors.
  expect_warning(
    eval_vars <- heRomod2:::eval_variables(parsed_vars, test_ns),
    'Error in evaluation of parameters: "a", "c", "e", "g".'
  )
  
  # Check that the value of the parameters are heRo_error objects.
  expect_equal(class(eval_vars['a']), 'heRo_error')
  expect_equal(class(eval_vars['c']), 'heRo_error')
  expect_equal(class(eval_vars['e']), 'heRo_error')
  expect_equal(class(eval_vars['g']), 'heRo_error')
  
  # Check that the error messages print correctly
  expect_output(
    print(eval_vars['a'], 'Error: Variable "z" not found.')
  )
  expect_output(
    print(eval_vars['c'], 'Error: Error in dependency "a".')
  )
  expect_output(
    print(eval_vars['e'], 'Error: Error in dependency "c".')
  )
  expect_output(
    print(eval_vars['g'], 'Error: Error in dependency "c".')
  )
})

test_that('formula syntax errors are handled properly', {
  
  # Parse the variables specification
  parsed_vars <- heRomod2:::parse_variables(var_tests$invalid, segment)
  
  # Check that a warning is produced naming all the parameters generating
  # errors.
  expect_warning(
    eval_vars <- heRomod2:::eval_variables(parsed_vars, test_ns),
    'Error in evaluation of parameters: "a", "c", "e", "g".'
  )
  
  # Check that the value of the parameters are heRo_error objects.
  expect_equal(class(eval_vars['a']), 'heRo_error')
  expect_equal(class(eval_vars['c']), 'heRo_error')
  expect_equal(class(eval_vars['e']), 'heRo_error')
  expect_equal(class(eval_vars['g']), 'heRo_error')
  
  # Check that the error messages print correctly
  expect_output(
    print(eval_vars['a'], 'Error: Error in formula syntax.')
  )
  expect_output(
    print(eval_vars['c'], 'Error: Error in dependency "a".')
  )
  expect_output(
    print(eval_vars['e'], 'Error: Error in dependency "c".')
  )
  expect_output(
    print(eval_vars['g'], 'Error: Error in dependency "c".')
  )
})

test_that('variables are evaluated properly when sorted', {
  
  # Parse the variables specification
  parsed_vars <- heRomod2:::parse_variables(var_tests$sorted, segment)
  
  # Check that no warnings, errors, or outputs are produced
  # during evaluation.
  expect_silent(
    eval_vars <- heRomod2:::eval_variables(parsed_vars, test_ns)
  )
  
  # Check that parameter values are correct.
  expect_equal(unname(eval_vars['g']), 25.00623, tolerance = 1e-7)
  expect_equal(unname(eval_vars['b']), 200, tolerance = 1e-7)
  expect_equal(unname(eval_vars['a']), 100, tolerance = 1e-7)
  expect_equal(unname(eval_vars['c']), 101, tolerance = 1e-7)
  expect_equal(unname(eval_vars['e']), 301, tolerance = 1e-7)
  
})

test_that('variables are evaluated properly when unsorted', {
  
  # Parse the variables specification
  parsed_vars <- heRomod2:::parse_variables(var_tests$unsorted, segment)
  
  # Check that no warnings, errors, or outputs are produced
  # during evaluation.
  expect_silent(
    eval_vars <- heRomod2:::eval_variables(parsed_vars, test_ns)
  )
  
  # Check that parameter values are correct.
  expect_equal(unname(eval_vars['g']), 25.00623, tolerance = 1e-7)
  expect_equal(unname(eval_vars['b']), 200, tolerance = 1e-7)
  expect_equal(unname(eval_vars['a']), 100, tolerance = 1e-7)
  expect_equal(unname(eval_vars['c']), 101, tolerance = 1e-7)
  expect_equal(unname(eval_vars['e']), 301, tolerance = 1e-7)
  
})

test_that('missing column names in variables spec are caught', {
  
  # Parse the variables specification
  expect_error(
    heRomod2:::parse_variables(var_tests$missing_col, segment),
    
  )
  
})
