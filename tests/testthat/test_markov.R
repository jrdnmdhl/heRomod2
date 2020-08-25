context("Markov Models")

model <- system.file("models", "checkimab", package = "heRomod2") %>%
  read_model()

test_that('markov models are properly evaluated', {
  expect_success(results <- run_model(model))
})
