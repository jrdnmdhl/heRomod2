#' @export
print.heRovar <- function(x, ...) {
  cat(as.character(x), ...)
}

#' @export
print.heRovar_list <- function(x, ...) {
  for (i in seq_len(length(x))) {
    cat(
      crayon::red(names(x)[i]),
      crayon::red(': '),
      crayon::black(as.character(x[[i]])),
      '\n',
      sep = ''
    )
  }
}

#' @export
print.heRo_error <- function(x, ...) {
  print(x$message)
}
