
get_log_level <- function(log) {
  if (is.null(log)) {
    log <- list(
      level = 0,
      type = 'console'
    )
  }

  log
}

log_print_table <- function(tbl, log) {
  log <- get_log_level(log)
  if (log$level > 0) {
    switch(
      log$type,
      print(dplyr::as_data_frame(tbl), n = 10)
    )
  }
}
log_print_section_break <- function(log) {
  log <- get_log_level(log)
  if (log$level > 0) {
    switch(
      log$type,
      cat('\n\n\n______________________________\n\n\n')
    )
  }
}
log_print_heading <- function(txt, level, log) {
  log <- get_log_level(log)
  if (log$level > 0) {
    switch(
      log$type,
      switch(
        as.character(level),
        '1' = cat(crayon::underline(crayon::bold(paste0(txt, ': \n\n')))),
        '2' = cat(crayon::underline(paste0(txt,': \n'))),
        '3' = cat(paste0(txt,': \n\n'))
      )
    )
  }
}
