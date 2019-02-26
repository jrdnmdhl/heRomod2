#' Define a Namespace Object
#'
#' Creates a new namespace object which can be used
#' to store evaluated parameters.  A namespace object
#' combines a data frame for storing vector parameters
#' with an environment for storing non-vector parameters.
#'
#' @param df A data frame of pre-existing parameter values
#' @param parent An optional environment to serve as the
#' parent to the namespace environment
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
      model_day = model_time * cycle_length,
      model_week = model_day / days_per_unit('Weeks'),
      model_month = model_day / days_per_unit('Months'),
      model_year = model_day / days_per_unit('Years'),
      state_time = rep(seq_len(n_tunnels), each = n_cycles),
      state_day = state_time * cycle_length,
      state_week = state_day / days_per_unit('Weeks'),
      state_month = state_day / days_per_unit('Months'),
      state_year = state_day / days_per_unit('Years')
    ),
    env = env
  )
}

#' @export
clone_namespace <- function(x) {
  new <- x
  new$env <- rlang::env_clone(x$env)
  new
}

subset_namespace <- function(x, names) {
  copy <- clone_namespace(x)
  env_names <- get_names(copy, type = "env")
  df_names <- get_names(copy, type = "df")
  rm(list = intersect(env_names, names), envir = copy$env)
  copy$df <- copy$df[intersect(df_names, names)]
  copy
}

#' @export
merge.namespace <- function(old, new) {
  copy <- clone_namespace(old)
  walk(get_names(new, "df"), function(x) {
    copy$df[[x]] <- new$df[[x]]
  })
  walk(get_names(new, "env"), function(x) {
    copy$env[[x]] <- new$env[[x]]
  })
  copy
}

#' @export
update.namespace <- function(old, params, new) {
  evaluate(params, merge(old, new))
}

#' @export
export.namespaces <- function(x, ...) {

  # Extract names from namespaces
  env_names <- get_names(x$ns[[1]], "env", keywords = F)
  df_names <- get_names(x$ns[[1]], "df", keywords = F)
  all_names <- get_names(x$ns[[1]], "all", keywords = F)
  #exportNamespace(x, df_names, env_names)
  group_names <- unique(x$group)
  strat_names <- unique(x$strategy)

  n_vars <- length(all_names)
  n_groups <- length(group_names)
  n_strats <- length(strat_names)

  res_df <- dplyr::rowwise(x) %>%
    dplyr::do({
      df <- dplyr::as_data_frame(.$ns$df)
      df$group <- .$group
      df$strategy <- .$strategy
      df
    }) %>%
    ungroup() %>%
    tidyr::gather(
      name,
      value,
      -strategy,
      -group,
      -model_time,
      -state_time
    ) %>%
    dplyr::mutate(
      print = NA,
      summary = NA
    ) %>%
    select(name, strategy, group, model_time, state_time, value, print, summary)

  res_env <- dplyr::rowwise(x) %>%
    dplyr::do({
      df <- data_frame(
        name = env_names,
        strategy = .$strategy,
        group = .$group,
        model_time = NA,
        state_time = NA,
        value = NA,
        summary = NA,
        print = NA
      )
      for (i in seq_len(length(env_names))) {
        name <- env_names[i]
        export_res <- export(get(name, envir = .$ns$env))
        df$print[i] <- export_res$print
        df$summary[i] <- export_res$summary
      }
      df
    }) %>%
    ungroup() %>%
    select(name, strategy, group, model_time, state_time, value, print, summary)
  res <- data.table::rbindlist(list(res_df, res_env))
  res
  ### BOTTLENECK
  # find_factors <- res %>%
  #   dplyr::distinct(name, value, print, summary, .keep_all = T) %>%
  #   dplyr::group_by(name) %>%
  #   dplyr::summarize(
  #     group_dep = length(unique(group)) > 1,
  #     strategy_dep = length(unique(strategy)) > 1,
  #     state_time_dep = length(unique(state_time)) > 1,
  #     model_time_dep = length(unique(model_time)) > 1
  #   )
  # dplyr::left_join(res, find_factors, by = "name") %>%
  #   dplyr::group_by(name) %>%
  #   dplyr::filter(
  #     strategy_dep | strategy == strategy[1],
  #     group_dep | group == group[1],
  #     model_time_dep | model_time == model_time[1],
  #     state_time_dep | state_time == state_time[1]
  #   ) %>%
  #   dplyr::mutate(
  #     strategy = ifelse(strategy_dep, strategy, NA),
  #     group = ifelse(group_dep, group, NA),
  #     model_time = ifelse(model_time_dep, model_time, NA),
  #     state_time = ifelse(state_time_dep, state_time, NA)
  #   ) %>%
  #   dplyr::select(-group_dep, -strategy_dep, -state_time_dep, -model_time_dep) %>%
  #   dplyr::arrange(name)

}

#' @export
export.namespace <- function(x, ...) {

  # Extract names from namespaces
  env_names <- get_names(x, "env", keywords = F)
  df_names <- get_names(x, "df", keywords = F)
  all_names <- get_names(x, "all", keywords = F)

  n_vars <- length(all_names)

  if (ncol(x$df) > 2) {
    res_df <- x$df %>%
      tidyr::gather(
        name,
        value,
        -model_time,
        -state_time
      ) %>%
      dplyr::mutate(
        print = NA,
        summary = NA
      ) %>%
      select(name, model_time, state_time, value, print, summary)
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
    export_res <- export(get(name, envir = x$env))
    res_env$print[i] <- export_res$print
    res_env$summary[i] <- export_res$summary
  }

  res_env <- select(res_env, name, model_time, state_time, value, print, summary)
  res <- rbind(res_df, res_env)
  res
}


#' @export
export <- function(x, ...) {
  UseMethod("export", x)
}

#' @export
export.default <- function(x, ...) {
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
