
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
      print(dplyr::as_data_frame(tbl), n = 10),
      html = {
        print(
          kableExtra::kable_styling(
            knitr::kable(tbl),
            bootstrap_options = c("striped", "hover", "condensed"),
            font_size = 10
          ) %>%
          kableExtra::scroll_box(., extra_css = "max-height: 250px; height: auto; overflow: scroll !important;")
        )
        cat('\n\n')
      }
    )
  }
}
log_print_section_break <- function(log) {
  log <- get_log_level(log)
  if (log$level > 0) {
    switch(
      log$type,
      html = cat('***\n'),
      cat('\n\n\n______________________________\n\n\n')
    )
  }
}
log_print_heading <- function(txt, level, log) {
  log <- get_log_level(log)
  if (log$level > 0) {
    switch(
      log$type,
      html = switch(
        as.character(level),
        '1' = cat('### ' %+% txt %+% ': \n\n'),
        '2' = cat('#### ' %+% txt %+% ': \n\n'),
        '3' = cat('###### ' %+% txt %+% ': \n\n')
      ),
      switch(
        as.character(level),
        '1' = cat(crayon::underline(crayon::bold(txt %+% ': \n\n'))),
        '2' = cat(crayon::underline(txt %+% ': \n')),
        '3' = cat(txt %+% ': \n\n')
      )
    )
  }
}
