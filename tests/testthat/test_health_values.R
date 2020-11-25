# Test Health Values 
# ================
context("Health Values")
library(purrr)
library(dplyr)
library(lazyeval)

# Prepare data for test cases
health_val_tests <- system.file("test_cases", "test_health_values.xlsx", package = "heRomod2") %>%
  read_workbook() 
state_names <- health_val_tests$state$name

segment <- tibble::tibble(strategy = "S1", group = "G1")
test_ns <- heRomod2:::create_test_ns(segment)
cl_unit_code <- 'cycle_length_unit'
health_val_tests$settings <- heRomod2:::parse_settings(health_val_tests$settings)
n_cycles <- heRomod2:::get_n_cycles(health_val_tests$settings)
cycle_length_days <- heRomod2:::get_cycle_length_days(health_val_tests$settings)
t_states <- heRomod2:::parse_states(health_val_tests$state, health_val_tests$settings)
t_vars <- heRomod2:::parse_seg_variables(health_val_tests$variables, segment, trees = health_val_tests$trees)
t_health_values <- heRomod2:::parse_values(health_val_tests$state_values, t_vars)
context <- "Health Values"

x <-health_val_tests

# Check Health Values 
# ======================================
check_health_values <- function(x, ns, states, vars) {
  error_msg <- ''
  context <- "Health Values"
  
  # Check if states are in state table 
  # ----------------------------------
  rowwise(x) %>%
    group_split() %>%
    map(function(row, states) {
      flag <- row$state %in% states$name
      state <- row$state
      if (! flag) { error_msg <<- paste0(error_msg, state, " is not among model states.\n " )}
    }, t_states)
           
  # Check if type is one of "lifetable", "end", "start" 
  # ---------------------------------------------------
  type_list <- x$type %in% c('lifetable', 'end', 'start')
  if (!all(type_list)) {
    sel_index <- which(!type_list)
    sel_types <- x$type[sel_index]
    msg <- heRomod2:::err_name_string(sel_types)
    error_msg <- paste0(
      error_msg, 
      'type(s) differ from lifetable, end, start:',
      msg, ", line(s):", sel_index,
      '.'
    )
  }
  
  # Ceck formula 
  # ------------------
  # add list of health value variables to namespace  
  extra_vars <- unique(c(x$name, heRomod2:::heRo_keywords, vars$name))
  vardf <- data.frame(matrix(0,nrow=1, ncol=length(extra_vars)))
  colnames(vardf) <- extra_vars
  extended_ns_df <- cbind(test_ns$df[1,], vardf)
  
  rowwise(x) %>%
    group_split() %>%
    map(function(row, ns) {
      res <- heRomod2:::safe_eval(lazy_eval(as.lazy(row$formula), ns))
      if ("heRo_error" %in% class(res)) {
          error_msg <<- res$message
      }
      error_msg
    }, extended_ns_df) 
  
  if (!error_msg==''){stop(paste0(context," ", error_msg))}
} 

test_that('Check state  values', {
  expect_error(
    check_health_values(health_val_tests$state_values, test_ns, t_states, t_vars),
    "Health Values relapse_free_off is not among model states.\n relapse2 is not among model states.\n ", fixed=TRUE
  )
})

test_that('Check types in lifetable, end, start', {
  expect_error(
    check_health_values(health_val_tests$types, test_ns, t_states, t_vars),
    "Health Values type(s) differ from lifetable, end, start:\"lifetable2\", line(s):2.", fixed=TRUE
  )
})


test_that('Check formulas', {
  expect_error(
    check_health_values(health_val_tests$wrong_formula, test_ns, t_states, t_vars),
    "Health Values Error in felse(state_cycle > 3, 0.2, 0.3): could not find function \"felse\"\n", fixed=TRUE
  )
})


#check_health_values(health_val_tests$wrong_formula, test_ns, t_states, t_vars)


