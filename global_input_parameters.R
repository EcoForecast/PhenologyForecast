global_input_parameters <- list(
  data.start.date = "2000-01-01", # the first day to look for phenocam and MODIS data
                                  # should be a january 1st
  model.start.DOY = 182, # the day of year that our model uses to define the begining of fall phenology
  number.of.SS.model.iterations = 1000, # the number of iterations per chain used in JAGS for the state-space models
  number.of.SS.model.chains = 3, # the number of chains used in JAGS for the state-space model
  x_ic=1, # state-space model JAGS input for the initial phenological state each summer (between 0 and 1)
  tau_ic=0.05, # state-space model JAGS input for the precision of our initial x state
  a_ndvi=3.16, # state-space model JAGS input for gamma shape parameter for ndvi prior
  r_ndvi=.316, # state-space model JAGS input for gamma rate parameter for ndvi prior
  a_gcc=3.16, # state-space model JAGS input for gamma shape parameter for gcc prior
  r_gcc=.316, # state-space model JAGS input for gamma rate parameter for gcc prior
  a_add=1.41, # state-space model JAGS input for gamma shape parameter for process model
  r_add=.71 # state-space model JAGS input for gamma rate parameter for process model
  )
