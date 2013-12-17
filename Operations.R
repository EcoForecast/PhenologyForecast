# A high-level outline of forecasting structure
setwd("/var/www/ge585/PhenologyForecast/")

# For each site, we need to run a state-space model, and then the forecast model for the current year
for(site in 1:5) {
  
  # Step 1: Download/update site GCC and MODIS data
  source("get.site.data.R")
  get.site.data(site)
  
  # Step 2: check to see if state space model has already been run
  source("check.for.complete.SS.model.R")
  SS.complete <- check.for.complete.SS.model(site) # SS.complete is TRUE/FALSE 
  
  # If not, then create the state space model for that site:
  if(!SS.complete) {
    source("run.SS.model.R")
    run.SS.model(site) # This should create some files which contain inputs for the forecast model
  }
  
  # Step 3: Check to see if the forecast model has been run (and has some output)
  # "FM" means forecast model, the particle filter in this case
  source("check.for.FM.model.R")
  FM.complete <- check.for.FM.model(site) # FM.created is TRUE/FALSE 
  
  # If not, then create the forecast model for that site:
  if(!FM.complete) {
    source("create.FM.model.R")
    create.FM.model(site) # This should create some files with forecasts and some other output
  }
  
  # Step 4: Check for new data not yet included in forecast model:
  source("update.FM.model.R")
  update.FM.model(site) # This should update the output from the forecast model for any new data
  
}
