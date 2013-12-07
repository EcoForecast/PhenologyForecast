# A high-level outline of forecasting structure

# For each site, we need to run a state-space model, and then the forecast model for the current year
for(site in 1:5) {

  # Step 1: check to see if state space model has already been run
  SS.complete <- check.for.complete.SS.model(site) # SS.complete is TRUE/FALSE 
  
  # If not, then create the state space model for that site:
  if(!SS.complete) {
    run.SS.model(site) # This should create some files which contain inputs for the forecast model
  }
  
  # Step 2: Check to see if the forecast model has been run (and has some output)
  # "FM" means forecast model, the particle filter in this case
  FM.complete <- check.for.complete.FM.model(site) # FM.complete is TRUE/FALSE 
  
  # If not, then create the forecast model for that site:
  if(!FM.complete) {
    create.FM.model(site) # This should create some files with forecasts and some paramaters
    }
  
  # Step 3: Check for new data not yet included in forecast model:
  new.FM.data.exists = check.for.new.FM.data(site) # TRUE/FALSE
  
  if(new.FM.data.exists) {
    update.FM.model(site) # This should update the output from the forecast model for any new data
  }
    
  
}
