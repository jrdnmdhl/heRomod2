library(heRomod2)
model <- read_model('./inst/models/checkimab_large')
model2 <- model
model2$settings$value[1] <- 'markov2'

microbenchmark::microbenchmark(r = res <- run_model(model), rcpp = res <- run_model(model2), times=1)

