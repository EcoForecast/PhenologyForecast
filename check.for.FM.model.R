### # Step 3 of Operations Script: check to see if state space model has already been run
check.for.FM.model <- function(site.number,current.year = as.numeric(strftime(Sys.Date(),"%Y"))) {
  
  source("global_input_parameters.R")
  model = global_input_parameters$model
  
  ## if FM model already ran, it will have produced output for the site being run 
  # with the following file name
  filename <- last.date.filename <- paste("last.update.site", as.character(site.number), model,
                                          "txt",sep=".")

  ## checks to see if this file exists in the current directory 
  if(file.exists(filename)){
    
    ##check to see if it's a new year
    read.in <- source(filename)
    last.forecast.year <- as.numeric(strftime(as.Date(read.in$value), "%Y"))
    if(current.year > last.forecast.year){
      ## if it's a new year, delete the update file and start fresh
      file.remove(filename)
      return(FALSE)
    }
    return(TRUE)    
  }
  return(FALSE)
  
} 
