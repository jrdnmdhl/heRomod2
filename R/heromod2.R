#' @import dplyr
#' @import furrr
#' @import purrr
#' @import openxlsx
#' @importFrom lazyeval lazy lazy_dots lazy_eval as.lazy_dots as.lazy
NULL


#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe
#' @export
NULL



#' @importFrom crayon %+%
#' @name %+%
#' @rdname Concatenate character vectors
#' @export
NULL

heRo_keywords <- c("cycle_length_days", "cycle_length_weeks", "cycle_length_months", "cycle_length_years",
                   "model_time", "model_day", "model_week", "model_month", "model_year", "state_time", "state_week", "state_month",
                   "state_year", "group", "strategy", "simulation", "bc", "analysis_type")

error_codes <- list(
  generic = '#ERR: ',
  invalid_expression = '#ERR: Invalid Expression'
)

strat_var_code <- 'strategy'
group_var_code <- 'group'
global_var_codes <- c('global', '')

tf_unit_code <- 'timeframe_unit'
tf_code <- 'timeframe'

cl_unit_code <- 'cycle_length_unit'
cl_code <- 'cycle_length'
