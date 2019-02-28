context("Sampling")

test_that("Sampling Variables", {
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
      ~name, ~strategy, ~group, ~formula, ~sampling,
      'x'  , NA,        NA,     'y + 1', NA,
      'y'  , NA,        NA,     'z + 2', NA,
      'z'  , NA,        NA,     '30', 'normal(30, 3)',
      'a'  , NA,        NA,     'mtcars', 'bootstrap(mtcars)',
      'b'  , NA,        NA,     'lm(mpg~disp, data = a)', NA,
      'c'  , NA,        NA,     'predict(b, newdata = data.frame(disp = x))', NA,
      'i'  , NA,        NA,     'model_time + c', NA
    ),
    strategies = tribble(~name, 'strat1', 'strat2', 'strat3'),
    groups = tribble(
      ~name,     ~weight,
      'group_a', '0.5',
      'group_b', '0.3',
      'group_c', '0.2'
    )
  ) 
  res <- evaluate_model(model)

  samples <- resample(model, 10, res$segments)
  
  expect_equal(nrow(samples), 90)
  expect_equal(colnames(samples), c('simulation', 'strategy', 'group', 'z', 'a'))
  expect_equal(rep(32, 90), purrr::map_dbl(samples$a, nrow))
})
