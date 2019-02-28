#' Define a Namespace Object
#'
#' Creates a new namespace object which can be used
#' to store evaluated parameters.  A namespace object
#' combines a data frame for storing vector parameters
#' with an environment for storing non-vector parameters.
#'
#' @param df A data frame of pre-existing parameter values
#' @param env An environment of pre-existing values
#'
#' @export
define_namespace <- function(df, env) {
  define_object(df = df, env = env, class = 'namespace')
}

get_names <- function(ns, type = "all", keywords = T) {

  # Pull out names defined in ns
  if (type == "all") {
    res <- c(colnames(ns$df), ls(ns$env))
  } else if(type == "df") {
    res <- colnames(ns$df)
  } else if(type == "env") {
    res <- ls(ns$env)
  } else {
    stop("Invalid value for argument 'type'")
  }

  # Remove keywords
  if (!keywords) {
    res <- setdiff(res, heRo_keywords)
  }

  res
}

create_namespace <- function(n_cycles, n_tunnels, cycle_length, env) {
  
  env$cycle_length_days <- cycle_length
  env$cycle_length_weeks <- cycle_length / days_per_unit('Weeks')
  env$cycle_length_months <- cycle_length / days_per_unit('Months')
  env$cycle_length_years <- cycle_length / days_per_unit('Years')
  
  define_namespace(
    df = tibble::tibble(
      model_time = rep(seq_len(n_cycles), times = n_tunnels),
      model_day = .data$model_time * cycle_length,
      model_week = .data$model_day / days_per_unit('Weeks'),
      model_month = .data$model_day / days_per_unit('Months'),
      model_year = .data$model_day / days_per_unit('Years'),
      state_time = rep(seq_len(n_tunnels), each = n_cycles),
      state_day = .data$state_time * cycle_length,
      state_week = .data$state_day / days_per_unit('Weeks'),
      state_month = .data$state_day / days_per_unit('Months'),
      state_year = .data$state_day / days_per_unit('Years')
    ),
    env = env
  )
}

#' Clone a Namespace
#' 
#' Clones a namespace object.
#' 
#' @param x The namespace to be cloned.
#' 
#' @return An identical copy of the namespace.
#' 
#' @keywords internal
clone_namespace <- function(x) {
  new <- x
  new$env <- rlang::env_clone(x$env)
  new
}

#' @export
summary.namespace <- function(object, ...) {

  # Extract names from namespaces
  env_names <- get_names(object, "env", keywords = F)
  df_names <- get_names(object, "df", keywords = F)
  
  if (length(df_names) > 0) {
    res_df <- tidyr::gather_(
        object$df,
        'name',
        'value',
        gather_cols = df_names
      ) %>%
      dplyr::mutate(
        print = NA,
        summary = NA
      ) %>%
      select(
        name,
        .data$model_time,
        .data$state_time,
        .data$value,
        .data$print,
        .data$summary
      )
  } else {
    res_df <- data.frame()
  }
  res_env <- data_frame(
    name = env_names,
    model_time = NA,
    state_time = NA,
    value = NA,
    summary = NA,
    print = NA
  )
  for (i in seq_len(length(env_names))) {
    name <- env_names[i]
    export_res <- export(get(name, envir = object$env))
    res_env$print[i] <- export_res$print
    res_env$summary[i] <- export_res$summary
  }

  res_env <- select(
    res_env,
    .data$name,
    .data$model_time,
    .data$state_time,
    .data$value,
    .data$print,
    .data$summary
  )
  res <- rbind(res_df, res_env)
  res
}

#' Export a Variable
#' 
#' Generates output representing the evaluated value for a given variable. This
#' is done by outputing its class, and the result of its print and summary
#' methods.
#' 
#' @param x The object being exported.
#' 
#' @return a list containing the class of the result, and the output of its
#' print and summary methods.
#' 
#' @export
export <- function(x) {
  UseMethod("export", x)
}


#' @export
export.default <- function(x) {
  res <- list()
  res$class <- tryCatch({
    class(x)
  }, error = function(e) {
    NULL
  })
  res$print <- tryCatch({
    paste(capture.output(x, split = F), collapse = "\n")
  }, error = function(e) {
    NULL
  })
  res$summary <- tryCatch({
    paste(capture.output(summary(x)), collapse = "\n")
  }, error = function(e) {
    NULL
  })
  res
}

update_segment_ns <- function(x, newdata) {
  
  # Clone the namespace
  new_ns <- clone_namespace(x)
  
  # Remove existing vars from df
  names_to_clear <- dplyr::intersect(colnames(newdata), colnames(new_ns$df))
  new_ns$df <- dplyr::select(new_ns$df, .dots = -names_to_clear)
  
  # Assign new values
  purrr::iwalk(newdata, function(x, n) {
    assign(n, x[[1]], envir = new_ns$env)
  })
  
  # Return namespace
  new_ns
  
}

#' @export
`[.namespace` <- function(x, i, ...) {
  df_names <- get_names(x, 'df')
  if (i %in% df_names) return(x$df[[i]])
  
  env_names <- get_names(x, 'env')
  if (i %in% env_names) return(get(i, envir = x$env))
  
  stop('Object not found')
}
