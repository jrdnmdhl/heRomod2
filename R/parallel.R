set_parallel_mode <- function(mode = 'sequential') {
  if (is.null(mode)) mode <- 'sequential'
  switch(
    mode,
    'parallel' = future::plan(future::multiprocess),
    future::plan(future::sequential)
  )
}
