context("Markov Models")

model <- system.file("models", "checkimab_simple", package = "heRomod2") %>%
  read_model()

test_that('markov trace calculations work success case', {
  mat1 <- as.matrix(
    tribble(
      ~cycle, ~from, ~to, ~value,
           1,     1,   2,   0.25,
           1,     1,   3,   0.15,
           1,     1,   1,    -pi,
           1,     2,   2,    -pi,
           1,     2,   3,   0.15,
           1,     3,   3,    -pi,
           2,     1,   1,    -pi,
           2,     1,   2,   0.20,
           2,     1,   3,   0.10,
           2,     2,   2,    -pi,
           2,     2,   3,   0.1,
           2,     3,   3,    -pi,
           3,     1,   1,    -pi,
           3,     1,   2,   0.20,
           3,     1,   3,   0.10,
           3,     2,   2,    -pi,
           3,     2,   3,   0.1,
           3,     3,   3,    -pi,
    )
  )
  init1 <- c(0.8, 0.2, 0)
  nstate1 <- 3
  ncycle1 <- 3
  statenames1 <- c('a', 'b', 'c')
  mat1_c1 <- matrix(c(
    0.6, 0.25, 0.15,
    0, 0.85, 0.15,
    0, 0, 1
  ), nrow = 3, ncol = 3, byrow = TRUE)
  mat1_c23 <- matrix(c(
    0.7, 0.2, 0.1,
    0, 0.9, 0.1,
    0, 0, 1
  ), nrow = 3, ncol = 3, byrow = TRUE)
  expected_trace <- matrix(nrow = 4, ncol = 3)
  colnames(expected_trace) <- statenames1
  rownames(expected_trace) <- c(0:3)
  expected_trace[1, ] <- init1
  expected_trace[2, ] <- expected_trace[1, ] %*% mat1_c1
  expected_trace[3, ] <- expected_trace[2, ] %*% mat1_c23
  expected_trace[4, ] <- expected_trace[3, ] %*% mat1_c23

  res <- cppMarkovTransitionsAndTrace(
    mat1,
    init1,
    statenames1,
    3,
    3,
    -pi
  )

  res$uncondTransProb
  
  expect_equal(res$trace, expected_trace)

  uncondTransProbSumsByCycle <- res$uncondtransprod %>%
    as.data.frame() %>%
    group_by(cycle) %>%
    summarize(sum = sum(value))

  expect_equal(uncondTransProbSumsByCycle$sum, c(1,1,1))
  expect_equal(!any(res$errors), TRUE)

})

## WIP!
test_that('markov trace calculations work complement error', {
  mat1 <- as.matrix(
    tribble(
      ~cycle, ~from, ~to, ~value,
           1,     1,   2,   0.25,
           1,     1,   3,   0.15,
           1,     1,   1,    -pi,
           1,     2,   2,    -pi,
           1,     2,   3,   -pi,
           1,     3,   3,    -pi,
           2,     1,   1,    -pi,
           2,     1,   2,   0.20,
           2,     1,   3,   0.10,
           2,     2,   2,    -pi,
           2,     2,   3,   0.1,
           2,     3,   3,    -pi,
           3,     1,   1,    -pi,
           3,     1,   2,   0.20,
           3,     1,   3,   0.10,
           3,     2,   2,    -pi,
           3,     2,   3,   -pi,
           3,     3,   3,    -pi,
    )
  )
  init1 <- c(0.8, 0.2, 0)
  nstate1 <- 3
  ncycle1 <- 3
  statenames1 <- c('a', 'b', 'c')
  mat1_c1 <- matrix(c(
    0.6, 0.25, 0.15,
    0, 0.85, 0.15,
    0, 0, 1
  ), nrow = 3, ncol = 3, byrow = TRUE)
  mat1_c23 <- matrix(c(
    0.7, 0.2, 0.1,
    0, 0.9, 0.1,
    0, 0, 1
  ), nrow = 3, ncol = 3, byrow = TRUE)
  expected_trace <- matrix(nrow = 4, ncol = 3)
  colnames(expected_trace) <- statenames1
  rownames(expected_trace) <- c(0:3)
  expected_trace[1, ] <- init1
  expected_trace[2, ] <- expected_trace[1, ] %*% mat1_c1
  expected_trace[3, ] <- expected_trace[2, ] %*% mat1_c23
  expected_trace[4, ] <- expected_trace[3, ] %*% mat1_c23

  res <- cppMarkovTransitionsAndTrace(
    mat1,
    init1,
    statenames1,
    3,
    3,
    -pi
  )
  
  expect_equal(res$trace, expected_trace)

  uncondTransProbSumsByCycle <- res$uncondtransprod %>%
    as.data.frame() %>%
    group_by(cycle) %>%
    summarize(sum = sum(value))

  expect_equal(uncondTransProbSumsByCycle$sum, c(1, 1, 1))
  expect_equal(!any(res$errors), TRUE)

})

# test_that('markov models are properly evaluated', {
#   expect_silent(results <- run_model(model))
#   tmat <- matrix(c(0.5, 0.3, 0.2, 0, 0.65, 0.35, 0, 0, 1), ncol = 3, byrow = T)
#   init_vec <- matrix(c(1, 0, 0), nrow = 1)
#   trace <- init_vec
#   cycle_tmat <- tmat
#   for (i in 1:12) {
#     trace <- rbind(trace, init_vec %*% cycle_tmat)
#     cycle_tmat <- cycle_tmat %*% tmat
#   }
#   trace
#   expect_equal(results$segments$trace_and_values[[1]][[1]], trace)

#   corrected_trace <- (trace[-1, ] + trace[-nrow(trace), ]) / 2
#   lys <- rowSums(corrected_trace[,1:2]) * 1/12
#   qalys <- rowSums(corrected_trace * cbind(rep(0.8*1/12, nrow(corrected_trace)), rep(0.65*1/12, nrow(corrected_trace)), rep(0, nrow(corrected_trace))))
#   values <- cbind(lys, qalys)

#   res_values <- cbind(
#     lys = rowSums(results$segments$trace_and_values[[1]][[3]][,,1]),
#     qalys = rowSums(results$segments$trace_and_values[[1]][[3]][,,2])
#   )

#   values
#   expect_equal(res_values, values)

# })
