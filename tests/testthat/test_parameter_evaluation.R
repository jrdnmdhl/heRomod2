context("Parameter Evaluation")
library(tibble)

test_that("Evaluating Simple Variables", {
  vars <- tribble(
    ~name, ~formula,
    'x'  , 'y + 1',
    'y'  , 'z + 2',
    'z'  , '30',
    'i'  , 'model_time'
  )
  var_list <- heRomod2:::define_variable_list(vars) %>%
    sort()
  ns <- heRomod2:::define_namespace(data.frame(model_time = c(1,2,3)), new.env())
  var_res <- heRomod2:::evaluate_variable_list(var_list, ns)
  
  expect_equal(var_res['z'], 30)
  expect_equal(var_res['y'], 32)
  expect_equal(var_res['x'], 33)
  expect_equal(var_res['i'], c(1, 2, 3))
  
})

test_that("Evaluating Object Variables", {
  vars <- tribble(
    ~name, ~formula,
    'x'  , 'y + 1',
    'y'  , 'z + 2',
    'z'  , '30',
    'a'  , 'mtcars',
    'b'  , 'lm(mpg~disp, data = a)',
    'c'  , 'predict(b, newdata = data.frame(disp = x))',
    'i'  , 'model_time + c'
  )
  var_list <- heRomod2:::define_variable_list(vars) %>%
    sort()
  ns <- heRomod2:::define_namespace(data.frame(model_time = c(1,2,3)), new.env())
  var_res <- heRomod2:::evaluate_variable_list(var_list, ns)
  
  expect_equal(var_res['z'], 30)
  expect_equal(var_res['y'], 32)
  expect_equal(var_res['x'], 33)
  expect_equal(var_res['a'], mtcars)
  expect_equal(unname(var_res['c']), 28.23976, tolerance = 1e-5)
  expect_equal(unname(var_res['i']), c(1,2,3) + 28.23976, tolerance = 1e-5)
})

test_that("Error handling", {
  vars <- tribble(
    ~name, ~formula,
    'x'  , 'y + 1',
    'y'  , 'z + 2',
    'z'  , 'blah',
    'a'  , 'mtcars',
    'b'  , 'lm(mpg~disp, data = a)',
    'c'  , 'predict(b, newdata = data.frame(disp = x))',
    'i'  , 'model_time + c'
  )
  var_list <- heRomod2:::define_variable_list(vars) %>%
    sort()
  ns <- heRomod2:::define_namespace(data.frame(model_time = c(1,2,3)), new.env())
  var_res <- heRomod2:::evaluate_variable_list(var_list, ns)
  
  expect_equal(var_res['z'], "#ERR: ")
  expect_equal(var_res['y'], "#ERR: ")
  expect_equal(var_res['x'], "#ERR: ")
  expect_equal(var_res['a'], mtcars)
  expect_equal(unname(var_res['c']), "#ERR: ")
  expect_equal(var_res['i'], "#ERR: ")
})


test_that("Circular Reference", {
  vars <- tribble(
    ~name, ~formula,
    'x'  , 'y + 1',
    'y'  , 'z + 2',
    'z'  , 'x'
  )
  vars <- heRomod2:::define_variable_list(vars)
  expect_error(sort(vars), 'Circular')
})
