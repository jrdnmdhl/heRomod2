#
# Check Groups and Strategis tables 
# ================================
# Checking if names are allowed variable names and there are not duplicate names
# Chaking that all weights in group tables are numeric
# ===============================================================================

chek_groups_and_strategies <- function(x) {
  check_variables_df(x$groups, context = "Groups") 
  check_weights(x$groups$weight)
  check_variables_df(x$strategies, context = "Strategies")  
  #if (error_msg != '') stop(error_msg, call. = F)
}


check_weights <- function(x,context='Groups') {
  error_msg = ''
  if (!is.numeric(x)) {
    wgt_msg <- x[suppressWarnings(is.na(as.numeric(x)))]
    error_msg <- paste0(context, ' weight has not numeric value: ', wgt_msg, '.')
  }
  #error_msg 
  if (error_msg != '') stop(error_msg, call. = F)
}



