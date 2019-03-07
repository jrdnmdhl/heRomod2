
#' @export
evaluate_longform_matrix <- function(df, ns) {
  base_df <- ns$df
  base_env <- ns$env
  time_df <- select(base_df, .data$model_time, .data$state_time)
  
  eval_trans <- df %>%
    rowwise() %>%
    group_split() %>%
    map(function(trans_row) {
      # Populate at dataframe with time, from, to
      time_df$from <- trans_row$from
      time_df$to <- trans_row$to
      # Convert formula string to lazy expression and set its environment
      lazy_expr <- lazyeval::as.lazy(trans_row$formula)
      lazy_expr$env <- base_env
      # Evaluate it using the namespace dataframe
      time_df$value <- lazyeval::lazy_eval(lazy_expr, data = base_df)
      # Return
      time_df
    }) %>%
    dplyr::bind_rows()
  
  eval_trans
}

#' @export
expand_eval_longform_matrix <- function(df) {
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
  df$to <- df$.to_e
  df$from <- df$.from_e
  # Return
  df[, c('model_time', 'state_time', 'from', 'to', 'value')]
}

array_last_unique_val <- function(mat, st_dim) {
  dims <- dim(mat)
  n_dim <- length(dims)
  st_dim_l <- dims[st_dim]
  access_str <- paste0(
    'mat[',
    strrep(',', st_dim - 1),
    st_dim_l,
    strrep(',', n_dim - st_dim),
    ']'
  )
  last_mat <- eval(parse(text = access_str))
  diff_from_last <- apply(mat, st_dim, function(x) {
    all(x != last_mat)
  })
  if (!any(diff_from_last)) return(-1)
  max(which(diff_from_last))
}

#' @export
longform_to_wide_matrix <- function(df, state_names) {
  n_state <- length(state_names)
  n_time <- max(df$model_time)
  dims <- c(n_state, n_state, n_time)
  from_index <- as.numeric(factor(df$from, levels = state_names)) 
  to_index <- as.numeric(factor(df$to, levels = state_names))
  arr_index <- (df$model_time - 1) * n_state^2 + (to_index-1) * n_state + from_index
  arr <- numeric(n_time * n_state^2)
  arr[arr_index] <-  df$value
  arr <- array(arr, dim = dims)
  dimnames(arr) <- list(
    state_names,
    state_names,
    as.character(seq_len(n_time))
  )
  arr
}

#' @export
longform_to_array <- function(df, dimcols, value) {
  n_dims <- length(dimcols)
  index <- integer(nrow(df))
  lengths <- integer(n_dims)
  uniques <- vector(mode = 'list', length = n_dims)
  factors <- vector(mode = 'list', length = n_dims)
  for (i in seq_len(n_dims)) {
    uniques[[i]] <- unique(df[[dimcols[i]]])
    lengths[i] <- length(uniques[[i]])
    factors[[i]] <- as.numeric(factor(df[[dimcols[i]]], levels = uniques[[i]]))
  }
  for (i in seq_len(n_dims)) {
    if (i == 1) {
      index <- index + factors[[1]]
    } else {
      prev_indices <- seq_len(i - 1)
      multiplier <- prod(lengths[prev_indices])
      index <- index + (factors[[i]] - 1) * multiplier
    }
  }
  vec <- numeric(prod(lengths))
  vec[index] <- df[[value]]
  array(vec, dim = lengths)
}



