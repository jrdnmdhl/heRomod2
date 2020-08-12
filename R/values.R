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
df1 <- df %>%
    group_by(state) %>%
    group_split() %>%
    map(function(x) {
      state_ns <- eval_variables(x, clone_namespace(ns), T)
      state_res <- state_ns$df
      state_res$state <- x$state[1]
      if (simplify) {
        # Transform to matrix to check state-time-dependency
        
        state_res <- state_res[ ,c("state", "cycle", "state_cycle", x$name )]
        
        val_mat <- state_res %>%
          gather_("variable","value", x$name, na.rm = TRUE) %>%
          lf_to_arr(c('cycle', 'state_cycle'), 'value')
        state_res$max_st <- arr_last_unique(val_mat, 2)
      } else {
        state_res$max_st <- Inf
      }
      state_res[ ,c("state","cycle", "state_cycle", "max_st", x$name)]
    }) %>%
    bind_rows()
  df2 <- df1 %>% gather(key = "name", value="value", c(lys,qalys))
  df3 <- merge(df2, df[,c("state","name","type")], by=c("state","name"), all.x = TRUE, all.y = FALSE )
}

values_to_vmat <- function(df, state_names) {
  value_names <- setdiff(colnames(df), c('state', 'cycle', 'state_cycle', 'max_st'))
  
  lf <-  df %>%
    #gather_("variable","value", value_names) %>%
    mutate(e_state = factor(expand_state_name(state, state_cycle), levels = state_names))
  lf <- lf[,!(names(lf) %in% c("state_cycle", "max_st"))]
  #mat <- lf_to_arr(lf, c('cycle', 'e_state', 'variable'), 'value')
  
 mat <- list()
  for (tp in c("start", "end", "lifetable")) {
    lf$value1 <- ifelse(lf$type == tp, lf$value, 0)
    mat0 <- lf_to_arr(lf, c('cycle', 'e_state', 'name'), 'value1')  
    dimnames(mat0) <- list(
      unique(df$cycle),
      state_names,
      c("lys","qalys") 
    )
    mat[[tp]] <- mat0
    }
  mat
}

