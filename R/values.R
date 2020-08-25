parse_values <- function(x, extra_vars) {
  
  # Check that values definition is valid
  check_values_df(x)
  
  # Parse values and sort
  vars <- x %>%
    group_by(state) %>%
    do({
      as.data.frame(.) %>%
        select(name, display_name, description, state, type, formula) %>%
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
  value_names <- unique(df$name)
  
  eval_values_wide <- df %>%
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
            pivot_longer(!!x$name, names_to = "name", values_to = "value", values_drop_na = T) %>%
            lf_to_arr(c('cycle', 'state_cycle'), 'value')
          state_res$max_st <- arr_last_unique(val_mat, 2)
        } else {
          state_res$max_st <- Inf
        }
        state_res[ ,c("state","cycle", "state_cycle", "max_st", x$name)]
      }) %>%
      bind_rows()
  
  eval_values_long <- eval_values_wide %>%
    pivot_longer(!!value_names, names_to = "name", values_to = "value") %>%
    left_join(df[ ,c("state", "name", "type")], by = c("state", "name")) %>%
    select(state, name, type, cycle, state_cycle, max_st, value)

  eval_values_long
}

evaluate_values1 <- function(df, ns, simplify = F) {
  value_names <- unique(df$name)
  
  eval_values_wide <- df %>%
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
          pivot_longer(!!x$name, names_to = "name", values_to = "value", values_drop_na = T) %>%
          lf_to_arr(c('cycle', 'state_cycle'), 'value')
        state_res$max_st <- arr_last_unique(val_mat, 2)
      } else {
        state_res$max_st <- Inf
      }
      state_res[ ,c("state","cycle", "state_cycle", "max_st", x$name)]
    }) %>%
    bind_rows()
  
  eval_values_long <- eval_values_wide %>%
    pivot_longer(!!value_names, names_to = "name", values_to = "value") %>%
    group_by() %>%
    group_split() %>%
    map(function(x) {
      if (simplify) {
        # Transform to matrix to check state-time-dependency
        
        state_res <- state_res[ ,c("state", "cycle", "state_cycle", x$name)]
        
        val_mat <- state_res %>%
          pivot_longer(!!x$name, names_to = "name", values_to = "value", values_drop_na = T) %>%
          lf_to_arr(c('cycle', 'state_cycle'), 'value')
        state_res$max_st <- arr_last_unique(val_mat, 2)
      } else {
        state_res$max_st <- Inf
      }
      state_res[ ,c("state","cycle", "state_cycle", "max_st", x$name)]
    }) %>%
    left_join(df[ ,c("state", "name", "type")], by = c("state", "name")) %>%
    select(state, name, type, cycle, state_cycle, max_st, value)
  
  eval_values_long
}

values_to_vmat <- function(df, state_names) {
  
  value_names <- unique(df$name)
  types <- unique(df$type)
  
  lf <- mutate(df, e_state = factor(expand_state_name(state, state_cycle), levels = state_names))

  values_matrix <- types %>%
    setNames(types) %>%
    map(function(type, data) {
      data$value1 <- ifelse(data$type == type, data$value, 0)
      mat <- lf_to_arr(data, c('cycle', 'e_state', 'name'), 'value1')  
      dimnames(mat) <- list(
        unique(data$cycle),
        state_names,
        value_names 
      )
      mat
  }, data = lf)

  values_matrix
}

