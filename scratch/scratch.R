library(heRomod2)
library(bench)
model <- read_model('./inst/models/checkimab_large')
model2 <- model
model2$settings$value[1] <- 'markovold'

#res2 <- run_model(model2)
#microbenchmark::microbenchmark(r = res <- run_model(model), rcpp = res <- run_model(model2), times=1)

#mark(rcpp = res <- run_model(model2), max_iterations=1,check=F)
#mark(rcpp = res <- run_model(model),r = res <- run_model(model2), max_iterations=1,check=F)

#res <- run_model(model)

#res2 <- run_model(model2)

