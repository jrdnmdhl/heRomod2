library(heRomod2)
library(bench)
  model <- system.file("models", "checkimab", package = "heRomod2") %>%
    read_model()
  eval_model <- run_model(model)
# #mark(rcpp = res <- run_model(model), max_iterations=1,check=F)


# res <- run_model(model)

