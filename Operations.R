# A high-level outline of forecasting structure

# For each site, we need to run a state-space model, and then the forecast model for the current year
for(site in 1:5) {
  
  # Step 1: Download/update site GCC and MODIS data
  get.site.data(site)
  
  # Step 2: check to see if state space model has already been run
  SS.complete <- check.for.complete.SS.model(site) # SS.complete is TRUE/FALSE 
  
  # If not, then create the state space model for that site:
  if(!SS.complete) {
    run.SS.model(site) # This should create some files which contain inputs for the forecast model
  }
  
  # Step 3: Check to see if the forecast model has been run (and has some output)
  # "FM" means forecast model, the particle filter in this case
  FM.created <- check.for.FM.model(site) # FM.created is TRUE/FALSE 
  
  # If not, then create the forecast model for that site:
  if(!FM.complete) {
    inputs.for.updating.forecast <- create.FM.model(site) # This should create some files with forecasts and some paramaters
  }
  
  # Step 4: Check for new data not yet included in forecast model:
  update.FM.model(site,inputs.for.updating.forecast) # This should update the output from the forecast model for any new data
  
}