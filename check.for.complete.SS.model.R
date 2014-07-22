### # Step 2 of Operations Script: check to see if state space model has already been run
check.for.complete.SS.model <- function(site) {

  source("global_input_parameters.R")
  model = global_input_parameters$model
  
  ## if SS model already ran, it will have produced output for the site being run with the following file name
  ## site is a number 
  file_name = paste('Jags.SS.out.site',as.character(site), model, 'RData',sep=".")

  ## checks to see if this file exists in the current directory and if so function evaluates to TRUE
  return(file.exists(file_name))

} 