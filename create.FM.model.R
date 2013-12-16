particle.filter.FM <- function(site_num){
  
  source("SSLPM.r") ## Super Simple Logistic Model
  source("ciEnvelope.R")
  source("global_input_parameters.R")
  source("update.r.R")  
  source("find.extreme.GCC.NDVI.R")
  
  ########################################
  #### dummy values for debugging ########
  ########################################
  site_num=1
  #########################################
  #########################################
  
  ## set up model time frame
  model.start.DOY=global_input_parameters$model.start.DOY
  cur_date = Sys.Date()
  current.year = as.numeric(format(Sys.Date(), "%Y"))
  
  ### read in output from State Space Model for X and r
  file_name = paste('Jags.SS.out.site',as.character(site_num), 'RData',sep=".")
  load(file_name)
  X.from.SS = as.matrix(jags.out.all.years.array[,5,])
  r.from.SS = as.matrix(jags.out.all.years.array[,1,])  
  
  #initial values for each ensemble member (average of all years of historical data)
  X.orig=apply(X.from.SS,1,mean)
  r.orig=apply(r.from.SS,1,mean)
  
  #take ensemble size from the size of the SS fit ensemble
  ne=length(X.orig)
  
  #load GCC data
  gcc.data <- read.csv( sprintf("gcc_data_site%i.csv",site_num) )
  # Current year only
  years=as.numeric(strftime(gcc.data$date,"%Y"))
  current.year.gcc.data=subset(gcc.data,years == current.year)
  # fall only
  days=as.numeric(strftime(current.year.gcc.data$date,"%j"))
  fall.cy.gcc.data = subset(current.year.gcc.data,days >= model.start.DOY)
  
  #load NDVI data
  ndvi.data <- read.csv( sprintf("ndvi_data_site%i.csv",site_num) )
  # Current year only
  years=as.numeric(strftime(ndvi.data$date,"%Y"))
  current.year.ndvi.data=subset(ndvi.data,years == current.year)
  # fall only
  days=as.numeric(strftime(current.year.ndvi.data$date,"%j"))
  fall.cy.ndvi.data = subset(current.year.ndvi.data,days >= model.start.DOY)
  
  # Find max/min of ndvi and gcc over all years of record except current
  # outputs (ndvi_max,ndvi_min,gcc_max,gcc_min)
  max_min_ndvi_gcc = find.extreme.GCC.NDVI(site_num, as.numeric(strftime(global_input_parameters$data.start.date, "%Y")), 
                                           current.year-1, use.interannual.means=TRUE)
  ndvi_max = max_min_ndvi_gcc[1]
  ndvi_min = max_min_ndvi_gcc[2]
  gcc_max = max_min_ndvi_gcc[3]
  gcc_min = max_min_ndvi_gcc[4]
  
  # Rescale data to be between 0 and 1 (using max and min NDVI, GCC values from 
  # all years except current year):
  rescaled_NDVI <- (fall.cy.ndvi.data$ndvi-ndvi_min)/(ndvi_max-ndvi_min)
  rescaled_GCC <- (fall.cy.gcc.data$gcc.90-gcc_min)/(gcc_max-gcc_min) # using gcc90
  
  # find first non-NA value in fall ndvi
  II_ndvi = which(!is.na(fall.cy.ndvi.data$ndvi))
  date.first.ndvi.value = as.Date(fall.cy.ndvi.data$date[min(II_ndvi)])
  first.ndvi.value = rescaled_NDVI[min(II_ndvi)]
  
  # find first non-NA value in fall gcc
  II_gcc = which(!is.na(fall.cy.gcc.data$gcc.90))
  date.first.gcc.value = as.Date(fall.cy.gcc.data$date[min(II_gcc)])
  first.gcc.value = rescaled_GCC[min(II_gcc)]
  
  gcc.comes.first = date.first.gcc.value < date.first.ndvi.value 
  if(gcc.comes.first) {
    date.first.data.index = min(II_gcc)
    obs.first.date = date.first.gcc.value
  } else{
    date.first.data.index = min(II_ndvi)
    obs.first.date = date.first.ndvi.value
  }
  
  obs.first.data = c(rescaled_GCC[date.first.data.index], rescaled_NDVI[date.first.data.index])
  
  ## Create a filter with GCC and NDVI equally weighted 
  ## (uses only one data source if the other is NA)
  
  ## not sure how we will error for the particle weights, so I'm using 50% of the mean for now 
  particle.filter.sd = mean(obs.first.data, na.rm = TRUE)*.5
  
  ### resampling particle filter
  hist.r=list()  ## since we resample parameters, create a record of what values were used each step
  hist.r[[1]] = r.orig ## initial parameter conditions
  X.prior = X.orig  ## reset state to the initial values, not the final values from the previous ensemble
  r.prior = r.orig
  
  ## forward step
  output = as.matrix(SSLPM(X.prior,r.prior))
  X=output[,1]
  r=output[,2]
  
  ##################################
  ##################################
  X[1] = 0.5 # because something is wrong in SSLPM function returens NA in first spot
  ##################################
  ##################################
  
  ## calulate Likelihood (weights)
  # Lm = mean(X) ## model filter over obs period
  wt = dnorm(X,mean(obs.first.data,na.rm=TRUE),particle.filter.sd)
  
  ## resample 
  index = sample.int(ne,ne,replace=TRUE,prob=wt)
  X.new = X[index]
  r.new = r[index] # update.r(r,index) # Why was this a function?  
  hist.r[[2]] = r.new
  
  ## saves X.new and r.new output to use in updating forecast model
  X.output_file_name = paste('ForecastModel.X.out.site',as.character(site_num), 'RData',sep=".")
  save(X.new,file = X.output_file_name)
  r.output_file_name = paste('ForecastModel.r.out.site',as.character(site_num), 'RData',sep=".")
  save(r.new,file = r.output_file_name)
  
  ### save STATES 
  output_file_name = paste('ForecastModel.X.out.site',as.character(site_num), 'RData',sep=".")
  save(output,file = output_file_name)
  
  ## Extract and summarize particle.filter
  particle.filter.pr = t(output[,,1])
  particle.filter.ci = apply(particle.filter.pr,2,quantile,c(0.025,0.5,0.975))
  
  #### saves output so that it can appended to as the forecast iterates
  ph.output_file_name = paste('ForecastModel.out.site',as.character(site_num), 'RData',sep=".")
  save(particle.filter.pr,file = ph.output_file_name)
  
  #### save plot produced to PDF
  ## name of output file
  file_name = paste('ParticleFilterForecast',as.character(site_num), 'pdf',sep=".")
  ## saves as PDF
  pdf(file=file_name)
  
  ##plot filter
  plot(time,particle.filter.ci[2,],type='n',ylab="NDVI_GCC",xlab="Time")
  ciEnvelope(time,particle.filter.ci[1,],particle.filter.ci[3,],col="light grey")
  points(time,particle.filter)    
  
  ## ends plot output to PDF
  dev.off()
  
  ## name of initial ensemble forecast file
  print(sprintf('The particle filter forecast for site No %.f is saved as %s',site_num,file_name))
  
  ### need nt and sample for particle filter update
  inputs.for.updating.forecast <- c(nt,sample)
  return(inputs.for.updating.forecast)
  
}  