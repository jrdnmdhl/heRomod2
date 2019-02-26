

#' @export
read_workbook <- function(path) {
  sheet_names <- excel_sheets(path)
  names(sheet_names) <- sheet_names
  lapply(sheet_names, function(x) read_excel(path, sheet = x))
}

define_object <- function(..., class) {
  structure(list(...), class = class)
}

is_in_segment <- function(d, strat, grp, include_globals = T) {
  is_my_strat <- d$strategy == strat
  is_my_group <- d$group == grp
  not_strat_spec <- (is.na(d$strategy) | d$strategy == '') & include_globals
  not_group_spec <- (is.na(d$group) | d$group == '') & include_globals
  
  (is_my_strat | not_strat_spec) & (is_my_group | not_group_spec)
}

read_in_tables <- function(tables, env, log) {
  if (length(tables) > 0) {
    log_print_section_break(log)
    log_print_heading('Reading in Tabular Data', level = 2, log = log)
  }
  for (name in names(tables)) {
    assign(name, tables[[name]], envir = env)
    log_print_heading(name, level = 3, log = log)
    log_print_table(dplyr::as_data_frame(tables[[name]]), log = log)
  }
}

run_scripts <- function(scripts, env, log) {
  if (length(scripts) > 0) {
    log_print_section_break(log)
    log_print_heading('Running R Scripts', level = 2, log = log)
  }
  for (name in names(scripts)) {
    log_print_heading('Running: ' %+% name, level = 3, log = log)
    eval(parse(text = scripts[[name]]), envir = env)
  }
}

get_cycle_length_days <- function(settings) {
  days_per_cl_unit <- days_per_unit(settings$value[settings$setting == cl_unit_code][1])
  cl <- as.numeric(settings$value[settings$setting == cl_code][1])
  cl * days_per_cl_unit
}

get_segments <- function(model) {
  
  if (nrow(model$groups) == 0) {
    model$groups <- tibble::tibble(
      name = 'all',
      display_name = 'All Patients',
      description = 'All Patients',
      weight = 1,
      enabled = 1
    )
  }
  
  expand.grid(
    group = groups$name,
    strategy =  model$strategies$name,
    stringsAsFactors = F
  )
}

get_n_cycles <- function(settings) {
  days_per_tf_unit <- days_per_unit(settings$value[settings$setting == tf_unit_code][1])
  days_per_cl_unit <- days_per_unit(settings$value[settings$setting == cl_unit_code][1])
  tf <- as.numeric(settings$value[settings$setting == tf_code][1])
  cl <- as.numeric(settings$value[settings$setting == cl_code][1])
  
  floor((tf * days_per_cl_unit) / (cl * days_per_cl_unit))
}

days_per_unit <- function(unit) {
  switch(
    unit,
    "Days" = 1,
    "Weeks" = 7,
    "Months" = 365/12,
    "Years" = 365
  )
}
