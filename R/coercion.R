#' @export
as.heRovar_list <- function(x) {
  UseMethod('as.heRovar_list', x)
}

#' @export
as.heRovar_list.list <- function(x) {
  structure(x, class = 'heRovar_list')
}
