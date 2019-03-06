
#' @export
evaluate_longform_matrix <- function(df, ns) {
  base_df <- ns$df
  base_env <- ns$env
  time_df <- select(base_df, !!expr(model_time))
  
  df %>%
    rowwise() %>%
    group_split() %>%
    map(function(trans_row) {
        time_df$from <- trans_row$from
        time_df$to <- trans_row$to
        lazy_expr <- lazyeval::as.lazy(trans_row$formula)
        lazy_expr$env <- base_env
        time_df$value <- lazyeval::lazy_eval(lazy_expr, data = base_df)
        time_df
    }) %>%
    dplyr::bind_rows()
}

#' @export
longform_to_wide_matrix <- function(df, state_names) {
  n_state <- length(state_names)
  n_time <- max(df$model_time)
  dims <- c(n_state, n_state, n_time)
  df$.from_index <- as.numeric(factor(df$from, levels = state_names))
  df$.to_index <- as.numeric(factor(df$to, levels = state_names))
  arr <- fill3dArray(
    df$.from_index,
    df$.to_index,
    df$model_time,
    df$value,
    dims
  )
  array(arr, dim = dims)
}
