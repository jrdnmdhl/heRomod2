

#' @export
run_psa <- function(model, ...) {
  
  # Run base case model
  bc_params <- evaluate(model, log = dots$log)
  
  # Create the global sampling spec
  sampling_params <- model$variables %>%
    dplyr::filter(!is.na(sampling)) %>%
    
  
  # Sample the parameters
  
  # Split by specification
  
  # Run the models with new data passed in
  evaluate(model, newdata = sampled_params, log = dots$psa$log)
  
  # Format the results
  
}
