




# Helpers --------------------------------------------------------------

limit_state_time <- function(df, state_time_limits) {
  # Join data with state time limit
  left_join(df, state_time_limits, by = "state") %>%
    # Remove any entries that exceed limit
    filter(state_cycle <= st_limit)
}



#' Evaluate a Longform Transition Matrix
eval_trans_markov_lf <- function(df, ns, simplify = FALSE) {
  
  # Loop through each row in transitions, evaluate, then
  # combine results into a single dataframe
  rowwise(df) %>%
    group_split() %>%
    map(function(row, ns, simplify = F) {
        # Populate at dataframe with time, from, to
        time_df <- ns$df[ ,c('cycle', 'state_cycle')]
        time_df$from <- row$from
        time_df$to <- row$to
        time_df$from_state_group <- row$from_state_group
        time_df$to_state_group <- row$to_state_group
        time_df$share_state_time <- row$share_state_time
        time_df$value <- NA
        time_df$error <- NA
        
        # Evalulate transition formula
        value <- evaluate_formula(row$formula[[1]], ns)
        
        # Check if value was an error in evaluating the formula
        if (is_error(value)) {
          time_df$error <- value$message
        }
        
        # Check if value is numeric
        if (any(class(value) %in% c('numeric', 'integer'))) {
          time_df$value <- as.numeric(value)
        } else {
          # If not numeric, mark it as an error
          type <- class(value)[1]
          time_df$error <- glue('Result of formula was of type {type}, expected numeric.')
        }
        
        if (simplify) {
          # Transform to matrix to check st-dependency
          val_mat <- lf_to_arr(time_df, c('state_cycle', 'cycle'), 'value')
          time_df$max_st <- arr_last_unique(val_mat, 1)
        } else {
          time_df$max_st <- Inf
        }
        
        # Return
        time_df
    }, ns, simplify = simplify) %>%
    bind_rows()
}


#' Convert Lonform Transitions Table to Matrix
lf_to_tmat <- function(df) {
  df <- df %>%
    group_by(.data$from) %>%
    mutate(.max_st = max(.data$state_cycle)) %>%
    ungroup() %>%
    mutate(
      .end = .data$state_cycle == .data$.max_st,
      .from_e = expand_state_name(.data$from, .data$state_cycle)
    )
  lv_sg_i <- (!df$share_state_time) | (df$from_state_group != df$to_state_group)
  lv_i <- df$from != df$to & lv_sg_i
  ls_i <- df$.end & !lv_i
  nx_i <- !(lv_i | ls_i)
  df$.to_e <- NA
  df$.to_e[lv_i] <- expand_state_name(df$to[lv_i], 1)
  df$.to_e[ls_i] <- expand_state_name(df$to[ls_i], df$.max_st[ls_i])
  df$.to_e[nx_i] <- expand_state_name(df$to[nx_i], df$state_cycle[nx_i] + 1)
  e_state_names <- unique(df$.from_e)
  df$to <- factor(df$.to_e, levels = e_state_names)
  df$from <- factor(df$.from_e, levels  = e_state_names)
  df <- df[, c('cycle', 'state_cycle', 'from', 'to', 'value')]
  mat <- lf_to_arr(df, c('cycle', 'from', 'to'), 'value')
  dimnames(mat) <- list(
    unique(df$cycle),
    e_state_names,
    e_state_names
  )
  
  # Calculate complementary probabilities
  calc_compl_probs(mat)
}

#' Calculate complementary probabilities in an evaluated transition matrix
calc_compl_probs <- function(mat) {
  posC <- mat == C
  c_counts <- rowSums(posC, dims = 2)
  state_names <- dimnames(mat)[[2]]
  colnames(c_counts) <- state_names
  if (!all(c_counts <= 1)) {
    problem_states <- c_counts[, apply(c_counts, 2, function(z) any(z > 1))]
    problems <- lapply(seq_len(ncol(problem_states)), function(i) {
      cycles <- problem_states[ , i]
      problem_cycles <- which(cycles > 1)
      min_cycle <- min(problem_cycles)
      max_cycle <- max(problem_cycles)
      if (all(problem_cycles == min_cycle:max_cycle)) {
        problem_cycles = paste0(min_cycle, '-', max_cycle)
      }
      data.frame(
        state = colnames(problem_states)[i],
        cycles = paste(problem_cycles, collapse = ', '),
        stringsAsFactors = F
      )
    }) %>%
      dplyr::bind_rows() %>%
      as.data.frame()
    
    message <- paste0(
      'Error in transition matrix, keyword "C" used more than once per state:\n',
      paste(capture.output(problems), collapse = "\n")
    )
    stop(message, call. = F)
  }
  
  mat[posC] <- 0
  
  valC <- 1 - rowSums(mat, dims = 2)[which(posC, arr.ind = TRUE)[, -3]] 
  mat[posC] <- valC
  mat
}

check_matrix_probs <- function(mat) {
  
}

check_lf_markov_trans <- function(x, state_names) {
  error_msg <- ''

  # Check column names
  missing_cols <- check_missing_colnames(x, trans_markov_lf_columns)
  if (length(missing_cols) > 0) {
    plural <- if (length(missing_cols) > 1) 's' else ''
    missing_msg <- paste(missing_cols, collapse = ', ')
    error_msg <- glue('Transitions definition was missing column{plural}: {missing_msg}.')
  }
  
  # Check that all from states are represented
  missing_states <- which(!(state_names %in% x$from))
  if (length(missing_states) > 0) {
    missing_state_names <- state_names[missing_states]
    plural <- if (length(missing_state_names) > 1) 's' else ''
    missing_state_msg <- paste(missing_state_names, collapse = ', ')
    error_msg <- glue('Transitions definition missing state{plural}: {missing_state_msg}.')
  }
  
  # Check that no transitions are duplicated
  trans_names <- paste0(x$from, '→', x$to)
  dupe <- duplicated(trans_names)
  if (any(dupe)) {
    dupe_names <- unique(trans_names[dupe])
    plural <- if (length(dupe_names) > 1) 's' else ''
    dupe_msg <- paste(dupe_names, collapse = ', ')
    error_msg <- glue('Transitions definition contains duplicate enties for transition{plural}: {dupe_msg}.')
  }
  
  # Check that formulas are not blank
  blank_index <- which(any(x$formula == '' | is.na(x$formula)))
  if (length(blank_index) > 0) {
    plural <- if (length(blank_index) > 1) 's' else ''
    blank_names <- paste0(x$from[blank_index], '→', x$to[blank_index])
    blank_msg <- paste(blank_names, collapse = ', ')
    error_msg <- glue('Transitions definition contained blank formula for transitions{plural}: {blank_msg}.')
  }
  
  if (error_msg != '') stop(error_msg, call. = F)
}

# Type coercion methods
#' @export
as.lf_markov_trans <- function(x) {
  UseMethod('as.lf_markov_trans', x)
}

# lf_markov_trans => lf_markov_trans
#' @export
as.lf_markov_trans.lf_markov_trans <- function(x) x

# data.frame => lf_markov_trans
#' @export
as.lf_markov_trans.data.frame <- function(x) {
  class(x) <- c('lf_markov_trans', class(x))
  x
}
