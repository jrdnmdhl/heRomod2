context("Namespaces")
library(tibble)

# Need these for comparing names generated
empty <- character(0)
symdiff <- function(x, y) {
  setdiff( union(x, y), intersect(x, y))
}

test_that("Getting Names", {
  
  # Evaluate some parameters to create a populated namespace
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
  
  # Get names for all variables
  all_names <- heRomod2:::get_names(var_res, type = 'all', keywords = F)
  expect_equal(symdiff(all_names, vars$name), empty)
  
  # Include keywords
  all_names_kw <- heRomod2:::get_names(var_res, type = 'all', keywords = T)
  expect_equal(symdiff(all_names_kw, c('model_time', vars$name)), empty)
  
  # Get names for all df vars
  df_names <- heRomod2:::get_names(var_res, type = 'df', keywords = F)
  expect_equal(symdiff(df_names, 'i'), empty)
  
  # Get names for all env vars
  env_names <- heRomod2:::get_names(var_res, type = 'env', keywords = F)
  expect_equal(symdiff(env_names, symdiff('i', vars$name)), empty)
  
  # Invalid type argument
  expect_error(
    heRomod2:::get_names(var_res, type = 'blah', keywords = F),
    'Invalid value'
  )
  
})

test_that("Cloning", {
  
  # Evaluate some parameters to create a populated namespace
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
  clone <- clone_namespace(var_res)
  
  expect_equal(var_res$df, clone$df)
  expect_equal(as.list(var_res$env), as.list(clone$env))
  
})

test_that("Export", {
  
  # Evaluate some parameters to create a populated namespace
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
  
  exported <- export(var_res)
  
  expect_equal(
    paste(capture.output(var_res['a'], split = F), collapse = "\n"),
    exported$print[exported$name == 'a']
  )
  
  expect_equal(
    paste(capture.output(summary(var_res['b'])), collapse = "\n"),
    exported$summary[exported$name == 'b']
  )
  
})
