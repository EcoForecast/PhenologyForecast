### # Step 3 of Operations Script: check to see if state space model has already been run
check.for.FM.model <- function(site) {
  
  ## if FM model already ran, it will have produced output for the site being run with the following file name
  ## site is a number 
  file_name = paste('ForecastModel.out.site',as.character(site_num), 'RData',sep=".")
  
  ## checks to see if this file exists in the current directory and if so function evaluates to TRUE
  return(file.exists(file_name))
  
} 