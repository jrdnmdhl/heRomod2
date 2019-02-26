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

test_that("Cloning & Merging", {
  
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
  
  vars2 <- tribble(
    ~name,   ~formula,
    'blah1', '100',
    'blah2', '1000'
  )
  var_list2 <- heRomod2:::define_variable_list(vars2) %>%
    sort()
  ns2 <- heRomod2:::define_namespace(data.frame(model_time = c(1,2,3)), new.env())
  var_res2 <- heRomod2:::evaluate_variable_list(var_list2, ns)
  
  merged_ns <- merge(var_res, var_res2)
  
  all_names_merge <- heRomod2:::get_names(merged_ns, type = 'all', keywords = F)
  expect_equal(symdiff(all_names_merge, c(vars$name, vars2$name)), empty)
  
})
