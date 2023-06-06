library(heRomod2)
library(bench)
model <- read_model('./inst/models/checkimab_large')
model2 <- model
model2$settings$value[1] <- 'markov2'

#model$settings$value[2] <- 1
# model$groups$enabled <- c(T,F,F,F)
# model$strategies$enabled <- c(T,F,F)
#model2$settings$value[2] <- 1
# model2$groups$enabled <- c(T,F,F,F)
# model2$strategies$enabled <- c(T,F,F)
#mark(res <- run_model(model), max_iterations = 1)
#res2 <- run_model(model2)
#microbenchmark::microbenchmark(r = res <- run_model(model), rcpp = res <- run_model(model2), times=1)

#mark(rcpp = res <- run_model(model2), max_iterations=1,check=F)
mark(rcpp = res <- run_model(model2),r = res <- run_model(model), max_iterations=1,check=F)

