
#' Normal distribution
#'
#' The `normal` function can be used to sample a normally-distributed
#' parameter.
#'
#' @author Antoine Filipovic-Pierucci
#'
#' @param mean Mean of parameter
#' @param sd Standard deviation of parameter
#'
#' @export
normal <- function(mean, sd) {
  function(x) stats::qnorm(p = x, mean = mean, sd = sd)
}

#' Lognormal distribution
#'
#' The `lognormal` function can be used to sample a parameter
#' whose natural log is distributed normally.  Lognormal distribution
#' can be specified by providing the mean and standard deviation of the
#' parameter either on the natural or log scale by supplying the
#' appropriate arguments.
#'
#' @author Antoine Filipovic-Pierucci
#'
#' @param mean Mean of parameter
#' @param sd Standard deviation of parameter
#' @param meanlog Mean of `log(parameter)`
#' @param sdlog Standard deviation of `log(parameter)`
#'
#' @export
lognormal <- function(mean, sd, meanlog, sdlog) {
  if (missing(sdlog)) sdlog <- sqrt(log(1 + sd^2/mean^2))
  if (missing(meanlog)) meanlog <- log(mean) - sdlog^2/2

  function(x) stats::qlnorm(p = x, meanlog = meanlog, sdlog = sdlog)
}

#' Bootstrap sampling of a data frame
#'
#' The `bootstrap` function can be used to handle sampling
#' of tables of individual patient data by selecting observations
#' from the table with replacement
#'
#' @param x The data frame to be sampled
#' @param id An optional column name representing the unique identifier. If
#' not specified, each row is assumed to represent a unique observation.
#' @param strata An optional character vector of column names representing the
#' strata within which resampling should be performed.
#' @param weight An optional column name representing the probability weight for each
#' observation.
#'
#' @export
bootstrap <- function(x, id = NULL, strata = NULL, weight = NULL) {
  if (is.null(id)) {
    x$.id <- seq_len(nrow(x))
  }
  id <- ".id"
  function(q) {
    n <- length(q)
    resampled_df <- group_by_(x, .dots = strata) %>%
      do({

        # Get the set of unique observations
        unique <- distinct_(x, .dots = id)
        n_unique <- nrow(unique)

        # Handle the weights if provided
        if (is.null(weight)) {
          prob <- NULL
        } else {
          prob <- unique[[weight]]
        }

        # Resample data frame
        sampled_indices <- sample(seq_len(n_unique), n * n_unique, replace = T, prob = prob)
        sampled_df <- slice(unique, sampled_indices) %>%
          mutate(.sim = rep(seq_len(n), each = n_unique)) %>%
          select_(.dots = c(".sim", id)) %>%
          left_join(x, by = id)

        sampled_df
      }) %>%
      ungroup()

    sim_index <- resampled_df$.sim
    select(resampled_df, -.data$.sim) %>%
      split(sim_index)
  }
}

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
resample <- function(model, n, segments, corr = NULL) {
  
  # Get the sampled parameters and give each a unique
  # id based on name/strategy/group
  params_df <- dplyr::filter(
    model$variables,
    !is.na(.data$sampling)
  ) %>%
    dplyr::mutate(
      .id = paste0('.', .data$strategy, '.', .data$group, '.', .data$name)
    )
  
  n_var <- nrow(params_df)

  # Assume independence if missing correlation matrix
  if (is.null(corr)) {
    corr <- diag(rep(1, n_var))
  }

  # Sample uniform random
  mat_p <- stats::pnorm(mvnfast::rmvn(n = n, mu = rep(0, n_var), sigma = corr))
  
  # Prepopulate a list to store simulations
  cols <- vector(mode = "list", length = n_var + 1)
  cols[[1]] <- seq_len(n)
  names(cols) <- c('simulation', params_df$.id)

  # Fill list with sampled values
  for (i in seq_len(n_var)) {
    
    # Get the namespace for segment
    seg_ns <- filter(
      segments,
      is.na(params_df$strategy[i]) |
        params_df$strategy[i] == '' |
        params_df$strategy[i] == .data$strategy,
      is.na(params_df$group[i]) |
        params_df$group[i] == '' |
        params_df$group[i] == .data$group
    )$eval_vars[[1]]
    
    # Setup the parameter
    param <- lazyeval::as.lazy(params_df$sampling[i], rlang::env_clone(seg_ns$env))
    param$env$bc <- seg_ns[params_df$name[i]]
    
    # Evaluate and assign
    dist_func <- lazy_eval(param, data = seg_ns$df)
    cols[[i + 1]] <- dist_func(mat_p[ ,i])
  }
  
  # Make a data.frame with results
  sampling_df <- do.call(data_frame, cols)
  
  # Create Segments
  dplyr::rowwise(segments) %>%
    dplyr::do({
      x <- .data
      seg_vars <- params_df %>%
        dplyr::filter(is_in_segment(.data, x$strategy, x$group))
      
      dplyr::select_(sampling_df, .dots = c('simulation', seg_vars$.id)) %>%
        magrittr::set_colnames(., c('simulation', seg_vars$name)) %>%
        dplyr::mutate(strategy = x$strategy, group = x$group) %>%
        .[,c('simulation', 'strategy', 'group', seg_vars$name)]
      
    }) %>%
    dplyr::ungroup()
}
