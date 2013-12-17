create.FM.model <- function(site_num){
  # The function create.FM.model takes output from the state space model and
  # makes an initial (data free) forecast through the end of the year.
  # The forecast is plotted in a pdf begining with ParticleFilterForecast,
  # (with a site number and date appended). The output from the 
  # current forecast is saved in a file begining with ForecastModel.X.out (with a 
  # site number and date appended).
  
  ## set up model time frame
  source("global_input_parameters.R")
  model.start.DOY=global_input_parameters$model.start.DOY
  cur_date = Sys.Date()
  current.year = as.numeric(format(Sys.Date(), "%Y"))
  time = model.start.DOY:365  
  
  ### read in output from State Space Model for X and r
  file_name = paste('Jags.SS.out.site',as.character(site_num), 'RData',sep=".")
  load(file_name)
  # There are now two new variables loaded: SS.years is a vector of the years of 
  # the SS model, jags.out.all.years.array is a M x N x P array, where:
  # M is the number of chains times the number of iterations,
  # N is 4 + the number of days being modeled [r  tau_add  tau_gcc  tau_ndvi	X]
  # X will use (366-model.start.DOY columns),
  # P is the number of years being modeled (same as length(SS.years))
  
  X.from.SS = as.matrix(jags.out.all.years.array[,5,]) # M x P, first day prior
  r.from.SS = as.matrix(jags.out.all.years.array[,1,]) # M x P prior on r  
  
  # initial conditions for each ensemble member (average of all years of historical 
  # data)
  X.ic = apply(X.from.SS,1,mean)
  r.ic = apply(r.from.SS,1,mean)
  
  # take ensemble size from the size of the SS fit ensemble
  num.ensembles = length(X.ic)
  
  ### Analysis step:
  # No data! So none to be done yet!
  
  ### Resampling step:
  # No data -> no likelihood -> no resampling yet!
  
  # The array "output" will hold all of our X and r values for all ensemble members
  # and all days:
  output = array(NA,c(length(time), num.ensembles, 2)) ## initialize output
  
  # We need to seed it with our initial conditions:
  output[1,,1] = X.ic
  output[1,,2] = r.ic
  
  ###### Forecast loop:
  print(paste("Forecasting for initial particle filter for site",
              as.character(site_num)))
  source("SSLPM.R")
  for(t in 2:length(time)){
    X = output[t-1,,1]
    r = output[t-1,,2]
    ## forward step
    output[t,,] = SSLPM(X,r) # num.ensembles x 2
    
  }
  ##### end of forecast loop
  print("Initial forecasting complete.")
    
  ### save output (not sure if we want to save all of it... maybe just the most recent day's?)
  output_file_name = paste("ForecastModel.X.out.site", as.character(site_num),
                           "RData",sep=".")
  save(output,file = output_file_name)
  
  ## Plot our forecast!
  X.mat = output[,,1]
  X.ci  = apply(X.mat,1,quantile,c(0.025,0.5,0.975))
    
  #### save plot produced to PDF
  # Date of the last data point used in the forecast:
  date.string <- paste(as.character(current.year),
                       format(as.Date(model.start.DOY-2,origin="2001-01-01"), 
                              format="%m-%d"),sep="-") 
  # Complicated! But just a date string to put in the file name. For the create FM 
  # model it's the day BEFORE the first day of data.
  
  ## name of output file
  file_name = paste("ParticleFilterForecast",as.character(site_num),
                    as.character(date.string),"pdf",sep=".")
  
  
  ## saves as PDF
  pdf(file=file_name)
  
  ## plot forecast:
  plot(time,X.ci[2,],type='n',main=paste("Particle Filter Forecast:",date.string)
       ,xlab="Day of Year",ylab="Pheno-state")
  source("ciEnvelope.R")
  ciEnvelope(time,X.ci[1,],X.ci[3,],col="light grey")
  lines(time,X.ci[2,],main=paste("Particle Filter Forecast:",date.string)
        ,xlab="Day of Year",ylab="Pheno-state")
  
  ## ends plot output to PDF
  dev.off()
  
  ## name of initial ensemble forecast file
  print(sprintf("The particle filter forecast for site Num %.f is saved as %s",site_num,file_name))
  
  #### Save a file that contains the date of the last forecast:
  last.date.filename <- paste("last.update.site", as.character(site_num), 
                              "txt",sep=".")
  sink(last.date.filename, append = FALSE)
  cat("\"",date.string,"\"",sep="")
  sink()  
}  