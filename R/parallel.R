set_parallel_mode <- function(mode = 'sequential') {
  if (is.null(mode)) mode <- 'sequential'
  switch(
    mode,
    'parallel' = plan(multiprocess),
    plan(sequential)
  )
}
