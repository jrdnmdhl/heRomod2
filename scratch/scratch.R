#+ results='hide', echo=FALSE, warning=FALSE
library(heRomod2)
library(heRomod)
library(tibble)
st_max = 100
time_max = 100
n_states = 100
state_names <- paste0('state', 1:n_states)
trans <- data.frame(from=state_names, to=state_names)
trans$formula <- 'state_time'
hm_par <- define_parameters(a=1)
hm_par_e <- heRomod:::eval_parameters(hm_par, cycles = time_max)
state_names <- c(
  LETTERS,
  paste0("A", LETTERS),
  paste0("B", LETTERS),
  paste0("C", LETTERS)
)[1:n_states]
arg_list <- diag(1, n_states, n_states, names = F) %>%
  as.numeric %>%
  as.list %>%
  append(list(state_names = state_names))
hm_tmat <- do.call(define_transition, arg_list)



ns <- heRomod2:::create_namespace(time_max, st_max, 1, new.env())

testit <- eval_lf_matrix(trans, ns)
testit1 <- heRomod:::eval_transition.uneval_matrix(hm_tmat, hm_par_e, expand = tibble(
  .state = rep(state_names, each = st_max),
  state_time = rep(1:st_max, times = n_states),
  .full_state = paste0(.state, '.', state_time),
  .limit = st_max,
  .expand = T
))
microbenchmark::microbenchmark(
  a = {
    eval_lf_matrix(trans, ns)
  },
  b = {
    heRomod:::eval_transition.uneval_matrix(hm_tmat, hm_par_e, expand = tibble(
      .state = rep(state_names, each = st_max),
      state_time = rep(1:st_max, times = n_states),
      .full_state = paste0(.state, '.', state_time),
      .limit = st_max,
      .expand = T
    ))
  },
  times = 2
)
