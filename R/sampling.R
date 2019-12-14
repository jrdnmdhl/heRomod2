#' Resample Model Parameters
#' 
#' Resamples the parameters for a model based on the sampling specifications.
#' 
#' @param model A heRomodel object
#' @param n the number of simulations
#' @param segments the segments for which resampling will be done
#' @param corr an optional correlation matrix
#' 
#' @return a data.frame with resampled data by segment
#' 
#' @export
resample <- function(model, n, segments, corr = NULL, seed = NULL) {
  
  sampled_vars <- model$variables %>%
    filter(!is.na(sampling), sampling != '') %>%
    mutate(.index = seq_len(n()))
  n_var <- nrow(sampled_vars)
  var_names <- unique(sampled_vars$name)
  
  # Assume independence if missing correlation matrix
  if (is.null(corr)) {
    corr <- diag(rep(1, n_var))
  }
  
  # Sample uniform random
  mat_p <- stats::pnorm(mvnfast::rmvn(n = n, mu = rep(0, n_var), sigma = corr))
  
  # Prepopulate a list to store simulations
  cols <- vector(mode = "list", length = n_var + 1)
  cols[[1]] <- seq_len(n)
  #names(cols) <- c('simulation', params_df$name)
  
  # Fill list with sampled values
  for (i in seq_len(n_var)) {
    
    # Get the namespace of the relevant segment for variable
    var_row <- sampled_vars[i, ]
    var_name <- var_row$name
    row_strat <- var_row$strategy
    row_group <-  var_row$group
    is_ss <- !is.na(row_strat) && row_strat != ''
    is_gs <- !is.na(row_group) && row_group != ''
    if (!is_ss) row_strat <- segments$strategy[1]
    if (!is_gs) row_group <- segments$group[1]
    
    seg <- filter(segments, group == row_group, strategy == row_strat)
    seg_ns <- seg$eval_vars[[1]]
    seg_vars <- seg$uneval_vars[[1]]
    
    # Warn about strategy or group dependence
    deps <- seg_vars$formula[[which(seg_vars$name == var_name)]]$depends
    fo_deps <- seg_vars$formula[[which(seg_vars$name == var_name)]]$fo_depends
    if (('strategy' %in% deps) && !('strategy' %in% fo_deps )) {
      msg <- paste0(
        'Variable ',
        err_name_string(var_name),
        ' reference a strategy-dependent variable but was assigned a sampling distribution.',
        ' Sampling variables that reference strategy-dependent variables is not reccomended',
        ' and will result in a loss of strategy-dependence in PSA.'
      )
      warning(msg, call. = F)
    } else if (('group' %in% deps) && !('group' %in% fo_deps )) {
      msg <- paste0(
        'Variable ',
        err_name_string(var_name),
        ' reference a group-dependent variable but was assigned a sampling distribution.',
        ' Sampling variables that reference group-dependent variables is not reccomended',
        ' and will result in a loss of group-dependence in PSA.'
      )
      warning(msg, call. = F)
    }
    
    # Set up the parameter
    param <- lazyeval::as.lazy(var_row$sampling, rlang::env_clone(seg_ns$env))
    param$env$bc <- seg_ns[var_name]
    
    # Evaluate and assign
    dist_func <- lazy_eval(param, data = seg_ns$df)
    cols[[i + 1]] <- dist_func(mat_p[ ,i])
  }
  
  # Make a data.frame with sampled variables by segment
  seg_samples <- segments %>%
    rowwise() %>%
    group_split() %>%
    map(function(x) {
      indices <- sampled_vars %>%
        filter(is_in_segment(x, grp = group, strat = strategy)) %>%
        .$.index
      samp_list <- cols[c(1, indices + 1)] %>%
        setNames(c('simulation', var_names)) %>%
        append(list(strategy = x$strategy, group = x$group), after = 0)
      vars_df <- do.call(tibble, samp_list)
      vars_df
    }) %>%
    bind_rows
  seg_samples
}
