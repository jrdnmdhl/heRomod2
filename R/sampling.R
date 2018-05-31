
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
    select(resampled_df, -.sim) %>%
      split(sim_index)
  }
}

#' @export
resample <- function(x, n, ns, corr = NULL) {

  # Assume independence if missing correlation matrix
  if (is.null(corr)) {
    corr <- diag(rep(1, length(x)))
  }

  # Sample uniform random
  mat_p <- stats::pnorm(
    rmvn(
      n = n,
      mu = rep(0, length(x)),
      sigma = corr
    )
  )

  # Pre-allocate list of results
  cols <- vector(mode = "list", length = length(x))
  names(cols) <- names(x)

  # Fill list with sampled values
  for (i in seq_len(length(x))) {
    x[[i]]$env <- new.env(parent = ns$env)
    ns_df <- ns$df
    if (names(x)[i] %in% get_names(ns, "df")) {
      ns_df$bc <- ns_df[[names(x)[i]]]
    } else {
      x[[i]]$env$bc <- x[[i]]$env[[names(x)[i]]]
    }
    dist_func <- lazy_eval(x[[i]], data = ns_df[1, ])
    cols[[i]] <- dist_func(mat_p[ ,i])
  }

  # Turn into data frame
  do.call(data_frame, cols)

}