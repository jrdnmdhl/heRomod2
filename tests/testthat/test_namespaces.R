context("Namespaces")
library(tibble)



model <- system.file("models","checkimab", package="heRomod2") %>%
  read_model()

model$trees <- NULL
model$tables <- NULL
model <- heRomod2:::parse_model(model)
segment <- tibble(group = "male_age_lt_35", strategy = "chemo")
cycles <- rep(seq_len(360), 12)
rows <- length(cycles)
ns <- heRomod2:::create_namespace(
  model,
  segment
)


# Need these for comparing names generated
empty <- character(0)
symdiff <- function(x, y) {
  setdiff( union(x, y), intersect(x, y))
}

# Evaluate some parameters to create a populated namespace
vars <- tribble(
  ~name, ~formula,
  'x'  , 'y + 1',
  'y'  , 'z + 2',
  'z'  , '30',
  'a'  , 'mtcars',
  'b'  , 'lm(mpg~disp, data = a)',
  'c'  , 'predict(b, newdata = data.frame(disp = x))',
  'i'  , 'cycle + c'
)

vars$display_name <- ''
vars$description <- ''
vars$strategy <- segment$strategy
vars$group <- segment$group
var_list <- heRomod2:::parse_seg_variables(vars, segment)

test_that("Getting Names", {
  var_res <- heRomod2:::eval_variables(var_list, ns)
  
  # Get names for all variables
  all_names <- heRomod2:::get_names(var_res, type = 'all', keywords = F)
  expect_equal(symdiff(all_names, vars$name), empty)
  
  # Include keywords
  all_names_kw <- heRomod2:::get_names(var_res, type = 'all', keywords = T)
  expect_equal(symdiff(all_names_kw, c(heRomod2:::heRo_vars_keywords, vars$name)), empty)
  
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
  var_res <- heRomod2:::eval_variables(var_list, ns)
  clone <- heRomod2:::clone_namespace(var_res)
  
  expect_equal(var_res$df, clone$df)
  
  names(var_res$env) %>%
    magrittr::set_names(.,.) %>%
    as.list() %>%
    purrr::map(function(x) { 
      expect_equal(
        eval(parse(text = x), envir = var_res$env),
        eval(parse(text = x), envir = clone$env)
      )
    })
  
})

test_that("Summary", {
  
 
  var_res <- heRomod2:::eval_variables(var_list, ns)
  
  exported <- summary(var_res)
  
  expect_equal(
    paste(capture.output(var_res['a'], split = F), collapse = "\n"),
    exported$print[exported$name == 'a']
  )
  
  expect_equal(
    paste(capture.output(summary(var_res['b'])), collapse = "\n"),
    exported$summary[exported$name == 'b']
  )
  
})
