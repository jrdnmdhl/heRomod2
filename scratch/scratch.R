library(heRomod2)
library(bench)
  model <- system.file("models", "checkimab_large", package = "heRomod2") %>%
    read_model()
  res <- run_model(model)
# #mark(rcpp = res <- run_model(model), max_iterations=1,check=F)


# res <- run_model(model)



## TODO

#' MARKOV TRANSITIONS
#' - State-time limiting
#' - Shared state time
#' - Execute through errors
#' - Fractional Complements
#' 
#' Markov Values
#' - Residency vs. Transitional vs. Starting
#' - Positive/negative valence
#' - Per-value half-cycle correction methods
#' - Execute through errors