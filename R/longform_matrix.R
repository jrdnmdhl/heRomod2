
#' Evaluate a Longform Transition Matrix
#' @export
eval_lf_matrix <- function(df, ns, calc_max_st = TRUE) {
  df %>%
    rowwise() %>%
    group_split() %>%
    map(evaluate_trans_row, ns) %>%
    bind_rows() %>%
    group_by(from) %>%
    mutate(max_st = max(max_st)) %>%
    filter(state_time <= (max_st + 1)) %>%
    ungroup() %>%
    expand_lf_matrix()
}

#' Evaluate a row of a longform transition matrix
evaluate_trans_row <- function(row, ns, simplify = F) {
  # Populate at dataframe with time, from, to
  time_df <- ns$df[ ,c('model_time', 'state_time')]
  time_df$from <- row$from
  time_df$to <- row$to
  # Convert formula string to lazy expression and set its environment
  lazy_expr <- lazyeval::as.lazy(row$formula)
  lazy_expr$env <- ns$env
  # Evaluate it using the namespace dataframe
  time_df$value <- lazyeval::lazy_eval(lazy_expr, data = ns$df)
  if (simplify) {
    # Transform to matrix to check st-dependency
    val_mat <- lf_to_arr(time_df, c('state_time', 'model_time'), 'value')
    time_df$max_st <- arr_last_unique(val_mat, 1)
  } else {
    time_df$max_st <- Inf
  }
  
  # Return
  time_df
}

#' Expand an Evaluated Longform Matrix
#' @export
expand_lf_matrix <- function(df) {
  df <- df %>%
    group_by(.data$from) %>%
    mutate(.max_st = max(.data$state_time)) %>%
    ungroup() %>%
    mutate(
      .end = .data$state_time == .data$.max_st,
      .from_e = paste0(.data$from, '.', .data$state_time)
    )
  lv_i <- df$from != df$to
  ls_i <- df$.end & !lv_i
  nx_i <- !(lv_i | ls_i)
  df$.to_e <- NA
  df$.to_e[lv_i] <- paste0(df$to[lv_i], '.1')
  df$.to_e[ls_i] <- paste0(df$to[ls_i], '.', df$.max_st[ls_i])
  df$.to_e[nx_i] <- paste0(df$to[nx_i], '.', (df$state_time[nx_i] + 1))
  e_state_names <- unique(df$.from_e)
  df$to <- factor(df$.to_e, levels = e_state_names)
  df$from <- factor(df$.from_e, levels  = e_state_names)
  # Return
  df <- df[, c('model_time', 'state_time', 'from', 'to', 'value')]
  mat <- lf_to_arr(df, c('model_time', 'from', 'to'), 'value')
  dimnames(mat) <- list(
    unique(df$model_time),
    e_state_names,
    e_state_names
  )
  mat
}
