context("Logging")
library(tibble)

test_that("Logging a Simple Model", {
  model <- list(
    settings = tribble(
      ~setting,       ~value,
      "timeframe",         "72",
      "timeframe_unit",      "Years",
      "cycle_length",          "1",
      "cycle_length_unit",      "Years",
      "discount_cost",          "3",
      "discount_outcomes",          "3",
      "half_cycle_method", "life-table"
    ),
    variables = tribble(
      ~name, ~strategy, ~group, ~formula,
      'x'  , NA,        NA,     'y + 1',
      'y'  , NA,        NA,     'z + 2',
      'z'  , NA,        NA,     '30',
      'a'  , NA,        NA,     'mtcars',
      'b'  , NA,        NA,     'lm(mpg~disp, data = a)',
      'c'  , NA,        NA,     'predict(b, newdata = data.frame(disp = x))',
      'i'  , NA,        NA,     'model_time + c'
    ),
    strategies = tribble(~name, 'strat1', 'strat2', 'strat3'),
    groups = tribble(
      ~name,     ~weight,
      'group_a', '0.5',
      'group_b', '0.3',
      'group_c', '0.2'
    ),
    tables = list(
      mtcars = mtcars
    )
  )
  testthat::expect_output(
    evaluate_model(model, log = list(level = 2, type = 'console')),
    'Evaluating variables'
  )
  
  model1 <- heRomod2:::read_model(system.file('models', 'checkimab', package = 'heRomod2'))
  
  testthat::expect_output(
    evaluate_model(model1, log = list(level = 2, type = 'console')),
    '✅'
  )
  
  
})
