#+ results='hide', echo=FALSE, warning=FALSE
library(heRomod2)
# library(knitr)
# library(kableExtra)
# model <- read_workbook('~/Downloads/heRomod2_test/model.xlsx')
# model$tables <- list.files('~/Downloads/heRomod2_test/data') %>%
#   purrr::set_names(., gsub('.csv', '', ., fixed = T)) %>%
#   purrr::map(~read.csv(paste0('~/Downloads/heRomod2_test/data/', .), stringsAsFactor = F, check.names = F))
# model$scripts <- list.files('~/Downloads/heRomod2_test/scripts') %>%
#   purrr::set_names(., gsub('.R', '', ., fixed = T)) %>%
#   purrr::map(~readr::read_file(paste0('~/Downloads/heRomod2_test/scripts/', .)))
# class(model) <- "heRoModel"
# 
# #+ results='asis'
# res <- evaluate_model(model, log = list(level = 0,type = 'console'))
# 
# # sampl_list <- lazyeval::as.lazy_dots(model$variables$sampling) %>%
# #   purrr::set_names(model$variables$name) %>%
# #   .[!is.na(model$variables$sampling)]
# 
# hmm <- heRomod2::resample(model, 10, res$segments)
# future::plan(future::multiprocess)
# res2 <- evaluate_model(model, log = list(level=0,type='console', parallel_mode = 'parallel'), newdata = hmm)


df <- data.frame(
  model_time = 1:1000
)

state_names <- paste0('state', 1:100)
trans <- expand.grid(from = state_names, to = state_names, stringsAsFactors = F)
trans$formula <- 'exp(-model_time*0.05)'

ns <- heRomod2:::create_namespace(100, 1, 1, new.env())
testit <- evaluate_longform_matrix(trans, ns)
test1 <- longform_to_wide_matrix(testit, state_names)

a <- function(){
  trans %>%
    dplyr::rowwise() %>%
    dplyr::group_split() %>%
    purrr::map(
      function(x) {
        df$value <- lazyeval::lazy_eval(formula, data = df)
        df$from <- x$from
        df$to <- x$to
        df
      }
    ) %>%
    dplyr::bind_rows()
}

b <- function() {
  trans_names <- paste0(trans$from, '.', trans$to)
  formulas <- lazyeval::as.lazy_dots(purrr::set_names(rep(list(formula), length(trans_names)), trans_names))
  dplyr::mutate_(df, .dots = formulas)
}

microbenchmark::microbenchmark(a = a(), b = b(), times = 1)


#microbenchmark::microbenchmark(evaluate(model, log = list(level=0,type='console')), times=5)

#' test
# myFunc <- function() {
#   cat("#' ## Title")
# }
# 
# myFunc()
# 
# test_df <- data.frame(a = rep(5.424, 72))
# 
# lookup_tbl <- mtcars %>% dplyr::distinct(wt, .keep_all = T)
# 
# microbenchmark::microbenchmark(
#   a = look_up(lookup_tbl, wt = 5.424, value = 'gear'),
#   b = look_up(lookup_tbl, wt = test_df$a, value = 'gear')
# )


#
# define_params_context <- function(x, strat, grp) {
#   dplyr::filter(x,
#     (type == 'strategy variable' & identifier == strat) |
#       (type == 'group_variable' & identifier == grp) | type == 'parameter'
#   ) %>%
#     define_parameters()
# }
#
# context_param_names <- dplyr::filter(model$variables, type != 'parameter')$name
#
# the_contexts <- expand.grid(group = model$groups$name, strategy = model$strategies$name)
# first_res <- define_params_context(model$variables, the_contexts$strategy[1], the_contexts$group[1]) %>%
#   evaluate(heRomod2:::create_namespace(100,100))
#
# subseq_res <- the_contexts[-1, ] %>%
#   rowwise() %>%
#   do({
#
#   })
#   c("group", "strategy"), function(x) {
#   param_list <- define_params_context(model$variables, x$strategy, x$group)
#   first_context_index <- which(names(param_list) %in% context_param_names)[1]
#
#   # Remove variables before first context parameter if any found
#   if (!is.na(first_context_index)) {
#     param_list <- param_list[-seq_len(first_context_index - 1)]
#   }
#
#   evaluate(param_list, clone_namespace(first_res))
# })
