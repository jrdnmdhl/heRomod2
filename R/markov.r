parse_markov <- function(model) {
  define_object_(model, class = 'markov')
}

run_segment <- function(segment, model, env, ...) {
  UseMethod('run_segment', model)
}

run_segment.markov <- function(segment, model, env, ...) {
  
  # Capture the extra arguments provided to function
  dots <- list(...)
  
  # Count the required number of model cycles and the
  # cycle length in days
  n_cycles <- get_n_cycles(model$settings)
  cycle_length_days <- get_cycle_length_days(model$settings)
  
  # Parse the specification tables provided for states,
  # variables, transitions, values, and summaries
  uneval_states <- parse_states(model$states, model$settings)
  uneval_vars <- parse_seg_variables(model$variables, segment, trees = model$trees)
  uneval_trans <- parse_trans_markov(model$transitions, uneval_states, uneval_vars)
  uneval_health_values <- parse_values(model$health_values, uneval_vars)
  #uneval_econ_values <- parse_values(model$economic_values)
  
  # Check inside the variables, transitions, & values for
  # any state-time dependency since this will inform the
  # creation of tunnel states
  state_time_use <- check_state_time(
    uneval_vars,
    uneval_trans,
    uneval_health_values,
    uneval_health_values
  )

  # Create a "namespace" which will contain evaluated
  # variables so that they can be referenced.
  ns <- create_namespace(model, segment)
  
  # Evaluate variables, initial state probabilities, transitions,
  # values, & summaries.
  eval_vars <- eval_variables(uneval_vars, ns)
  eval_states <- eval_states(uneval_states, eval_vars)
  eval_trans <- eval_trans_markov_lf(uneval_trans, eval_vars, model$settings$reduce_state_cycle)
  eval_health_values <- evaluate_values(uneval_health_values, eval_vars, model$settings$reduce_state_cycle)
  
  # Determine the number of tunnel states that need to be created
  # for each state.
  st_maxes <- rbind(
    rename(eval_trans[ , c('from', 'max_st')], state = from),
    eval_health_values[ , c('state', 'max_st')]
  ) %>%
    group_by(state) %>%
    summarize(max_st = max(max_st))
  
  # Remove any calculated values for transitions & values that are
  # not needed.
  eval_trans_limited <- select(eval_trans, -max_st) %>%
    left_join(st_maxes, by = c('from' = 'state')) %>%
    filter(state_cycle <= max_st)
  eval_health_values_limited <- select(eval_health_values, -max_st) %>%
    left_join(st_maxes, by = c('state' = 'state')) %>%
    filter(state_cycle <= max_st)
  
  # Transform transitions and values to matrices.
  eval_trans_mat <- lf_to_tmat(eval_trans_limited)
  expanded_state_names <- dimnames(eval_trans_mat)[[2]]
  eval_health_values_mat <- values_to_vmat(eval_health_values_limited, expanded_state_names)
  
  # Calculate Trace Probabilities
  expand_init <- expand_init_states(eval_states, st_maxes)
  trace <- markov_trace(expand_init, eval_trans_mat)

  outcomes <- markov_outcome(eval_health_values_mat, trace$trace_hc$lifetable)
  
  # Create the object to return that will summarize the results of
  # this segment.
  segment$uneval_vars <- list(uneval_vars)
  segment$eval_vars <- list(eval_vars)
  segment$eval_trans <- list(eval_trans_limited)
  segment$eval_values <- list(eval_health_values_limited)
  segment$inital_state <- list(eval_states)
  segment$tmat <- list(eval_trans_mat)
  segment$vmat <- list(eval_health_values_mat)
  segment$trace <- list(trace)
  segment$outcomes <- list(outcomes)
  segment
}

markov_trace <- function(init, matrix) {
  
  # Prepare matrix to store trace & patient flows
  n <- dim(matrix)[1] + 1
  s <- length(init)
  trace_uncorr <- matrix(rep(init, each = n), nrow = n)
  colnames(trace_uncorr) <- colnames(init)
  
  # Prepare matrix to store patient flows
  flows <- array(0, c(n - 1, s, s), dimnames = list(c(), c(colnames(init)), c(colnames(init))))
  
  # Caclulate trace and flows cycle-by-cycle
  for (i in 2:n) {
    j <- i - 1
    trace_uncorr[i,] <- trace_uncorr[j,] %*% matrix[j,,]
    flows[j,,] <- trace_uncorr[j,] * matrix[j,,]
  }
  
  # Calculate half-cycle corrected versions of trace
  trace_hc <- calc_trace_hc(trace_uncorr)
  
  # Return trace and flows in list
  list(
    "trace" = trace_uncorr,
    "trace_hc" = trace_hc, 
    "flows" = flows
  )
}

markov_outcome <- function(weight_matrix, state_matrix) {
  m <- dim(weight_matrix)[3]
  res_mat <- replicate(m,state_matrix)
  res_mat <- weight_matrix * res_mat
}

# Markov
#
# Markov models specified using longform table structure for
# transition matrices.
# --------------------------------------------------

# Parse a Markov Transitions Specification
#
# Takes a longform transitions specifications table, a states
# specification, and a variables specification and returns
# an unevalted transitions object.
parse_trans_markov <- function(x, states, vars) {
  
  # Extract the state names
  state_names <- states$name
  
  # Check transitions definition
  check_trans_markov(x, state_names)
  
  # Construct the transitions object
  x$formula <- map(x$formula, as.heRoFormula)
  x$name <- paste0(x$from, '→', x$to)
  res <- sort_variables(x, vars) %>%
    select(name, from, to, formula) %>%
    left_join(
      transmute(
        states, name = name,
        from_state_group = state_group
      ),
      by = c('from' = 'name')
    ) %>%
    left_join(
      transmute(
        states,
        name = name,
        to_state_group = state_group,
        share_state_time = share_state_time
      ),
      by = c('to' = 'name')
    )
  
  # Return result
  as.lf_markov_trans(res)
}

# Check a Markov Transitions Specification
#
# Takes a Markov transitions specifications table and a
# vector of state names and checks that the specification is
# valid.
check_trans_markov <- function(x, state_names) {
  
  error_msg <- ''
  
  # Check column names
  missing_cols <- check_missing_colnames(x, trans_markov_cols)
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
      value <- eval_formula(row$formula[[1]], ns)
      
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

#' Calculate half-cycle corrected versions of trace
calc_trace_hc <- function(mat) {
  trace_start <- mat[-nrow(mat), ]
  trace_end <- mat[-1, ]
  trace_lifetable <- (trace_start + trace_end) / 2
  list(
    "start" = trace_start,
    "end" = trace_end,
    "lifetable" = trace_lifetable
  )
}

check_matrix_probs <- function(mat) {
  
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


