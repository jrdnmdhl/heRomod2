parse_values <- function(x, extra_vars) {
  
  # Check that values definition is valid
  check_values_df(x)
  
  # Parse values and sort
  vars <- x %>%
    group_by(state) %>%
    do({
      as.data.frame(.) %>%
        select(name, display_name, description, state, formula) %>%
        mutate(formula = map(formula, as.heRoFormula)) %>%
        sort_variables(extra_vars)
    }) %>%
    ungroup()
  
  # Construct Object & Return
  as.values(vars)
}

check_values_df <- function(x) {
  
}

as.values <- function(x) x

#' @export
evaluate_values <- function(df, ns, simplify = F) {
  df %>%
    group_by(state) %>%
    group_split() %>%
    map(function(x) {
      state_ns <- eval_variables(x, clone_namespace(ns), T)
      state_res <- state_ns$df
      state_res$state <- x$state[1]
      if (simplify) {
        # Transform to matrix to check state-time-dependency
        
        state_res <- state_res[ ,c("state", "cycle", "state_cycle", x$name)]
        
        val_mat <- state_res %>%
          gather_("variable","value", x$name) %>%
          lf_to_arr(c('cycle', 'state_cycle'), 'value')
        state_res$max_st <- arr_last_unique(val_mat, 2)
      } else {
        state_res$max_st <- Inf
      }
      state_res[ ,c("state", "cycle", "state_cycle", "max_st", x$name)]
    }) %>%
    bind_rows()
}

values_to_vmat <- function(df, state_names) {
  value_names <- setdiff(colnames(df), c('state', 'cycle', 'state_cycle', 'max_st'))
  
  lf <-  df %>%
    gather_("variable","value", value_names) %>%
    mutate(e_state = factor(expand_state_name(state, state_cycle), levels = state_names))
  mat <- lf_to_arr(lf, c('cycle', 'e_state', 'variable'), 'value')
  dimnames(mat) <- list(
    unique(df$cycle),
    state_names,
    value_names
  )
  
  mat
    
}

