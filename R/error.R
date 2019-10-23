define_error <- function(x) {
  define_object(
    message = modify_error_msg(as.character(x)),
    class = 'heRo_error'
  )
}

modify_error_msg <- function(x) {
  x <- gsub("Error in eval(x$expr, data, x$env): ", "", x, fixed = T)
  x <- gsub("Error: ", "", x, fixed = T)
  if (grepl('object.*not found', x)) {
    name <- strsplit(strsplit(x, ': object ')[[1]][2], "'")[[1]][2]
    x <- glue('Error, variable "{name}" not found.')
  }
  x
}

# Determine whether a given object is an error
is_error <- function(x) {
  'heRo_error' %in% class(x)
}
