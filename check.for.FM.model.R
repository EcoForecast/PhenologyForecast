### # Step 3 of Operations Script: check to see if state space model has already been run
check.for.FM.model <- function(site_num) {
  
  ## if FM model already ran, it will have produced output for the site being run 
  # with the following file name
  filename <- last.date.filename <- paste("last.update.site", as.character(site_num), 
                                          "txt",sep=".")
  
  ## checks to see if this file exists in the current directory and if so function evaluates to TRUE
  return(file.exists(filename))
  
} 