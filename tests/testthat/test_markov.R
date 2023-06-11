context("Markov Models")

model <- system.file("models", "checkimab_simple", package = "heRomod2") %>%
  read_model()

test_that('markov models are properly evaluated', {
  expect_silent(results <- run_model(model))
  tmat <- matrix(c(0.5, 0.3, 0.2, 0, 0.65, 0.35, 0, 0, 1), ncol = 3, byrow = T)
  init_vec <- matrix(c(1, 0, 0), nrow = 1)
  trace <- init_vec
  cycle_tmat <- tmat
  for (i in 1:12) {
    trace <- rbind(trace, init_vec %*% cycle_tmat)
    cycle_tmat <- cycle_tmat %*% tmat
  }
  trace
  expect_equal(results$segments$trace_and_values[[1]][[1]], trace)

  corrected_trace <- (trace[-1, ] + trace[-nrow(trace), ]) / 2
  lys <- rowSums(corrected_trace[,1:2]) * 1/12
  qalys <- rowSums(corrected_trace * cbind(rep(0.8*1/12, nrow(corrected_trace)), rep(0.65*1/12, nrow(corrected_trace)), rep(0, nrow(corrected_trace))))
  values <- cbind(lys, qalys)

  res_values <- cbind(
    lys = rowSums(results$segments$trace_and_values[[1]][[3]][,,1]),
    qalys = rowSums(results$segments$trace_and_values[[1]][[3]][,,2])
  )

  values
  expect_equal(res_values, values)

})
