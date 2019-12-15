context("Sampling")

test_that("Sampling Variables", {
  
  model <- system.file("models", "checkimab", package = "heRomod2") %>%
    read_model()
  
  eval_model <- run_model(model)
  
  sampled_segments <- heRomod2:::resample(model, 10, eval_model$segments, seed = 1)
  
  expect_equal(c(29.576668, 25.957943, 28.716488), sampled_segments$start_age[1:3], tolerance = 1e-4)
  expect_equal(c(0.0014129674, 0.0009971054, 0.0008029979), sampled_segments$p_death_ae[1:3], tolerance = 1e-4)
  expect_equal(c(909.0764, 1490.6893, 584.2407), sampled_segments$cost_nausea[1:3], tolerance = 1e-4)
})
