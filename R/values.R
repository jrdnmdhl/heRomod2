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
      state_res$state <- x$state
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

evaluate_value_row <- function(df, ns, simplify = F) {
  # Populate at dataframe with time, state
  values_df <- ns$df
  values_df$name <- df$name
  values_df$state <- df$state
  state_ns <- ns
  state_ns$df <- values_df
  values_df$value <- evaluate_formula(df$formula[[1]], state_ns)
  if (simplify) {
    # Transform to matrix to check state-time-dependency
    val_mat <- lf_to_arr(values_df, c('cycle', 'state_cycle'), 'value')
    values_df$max_st <- arr_last_unique(val_mat, 1)
  } else {
    values_df$max_st <- Inf
  }
  
  # Return
  filter(values_df[ , c('name', 'state', 'cycle', 'state_cycle', 'value')], value > 0)
}
