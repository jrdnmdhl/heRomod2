
get_log_level <- function(log) {
  if (is.null(log)) {
    log <- list(
      level = 0,
      type = 'console'
    )
  }

  log
}

log_print_table <- function(tbl, log, indent = 0) {
  log <- get_log_level(log)
  if (log$level > 0) {
    switch(
      log$type,
      cat(
        paste(
          paste0(
            strrep('  ', indent),
            stringi::stri_unescape_unicode(
              capture.output(print(as.data.frame(tbl)))
            )
          ),
          collapse = '\n'
        ) %&% '\n'
      )
    )
  }
}
log_print_section_break <- function(log) {
  log <- get_log_level(log)
  if (log$level > 0) {
    switch(
      log$type,
      cat('\n')
    )
  }
}
log_print_heading <- function(txt, level, log) {
  log <- get_log_level(log)
  if (log$level > 0) {
    indent = strrep('  ', level - 1)
    switch(
      log$type,
      switch(
        as.character(level),
        '1' = cat(indent %&% crayon::underline$bold(txt %&% ':') %&% '\n'),
        '2' = cat(indent %&% crayon::bold(txt %&% ':') %&% ' \n'),
        '3' = cat(indent %&% crayon::bold(txt %&% ':') %&% '\n'),
        '4' = cat(indent %&% crayon::bold(txt %&% ':') %&% '\n'),
        '5' = cat(indent %&% crayon::bold(txt %&% ':') %&% '\n')
      )
    )
  }
}
