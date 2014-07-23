create.FM.model <- function(site.number,current.year = as.numeric(format(Sys.Date(), "%Y"))){
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
  print(current.year)
#  current.year = as.numeric(format(Sys.Date(), "%Y"))
#  if(!is.null(global_input_parameters$training.end.date)){
#    start.year = (as.numeric(strftime(global_input_parameters$training.end.date,"%Y"))+1)
#  } else {
#    start.year = current.year
#  }
  time = model.start.DOY:365  
  
  num.ensembles = global_input_parameters$num.ensembles

  model = global_input_parameters$model
  
  
  ### read in output from State Space Model for X and r
  file_name = paste('Jags.SS.out.site',as.character(site.number),model, 'RData',sep=".")
  load(file_name)
  # There are now two new variables loaded: 
  # SS.years is a vector of the years of the SS model
  # out is list:
  #   parms N_mcmc x N_parm array which are MCMC samples of the parameters
  #   ci    Year x Day x [hi, median, lo] matrix for the 95% CI and median for the historical state estimates
  
  # initial conditions for X
  X.bar = median(out$ci[,1,2])
  X.sd  = diff(range(out$ci[,1,]))/(1.96*2)
  X.ic =  pmax(0,pmin(1,rnorm(num.ensembles,X.bar,X.sd)))
    
  ### Analysis step:
  # No data! So none to be done yet!
  
  ### Resampling step:
  # No data -> no likelihood -> no resampling yet!
  
  # The matrix X will hold all of our values for all ensemble members
  # and all days:
  X = matrix(numeric(),length(time), num.ensembles) ## initialize output
  X[1,] = X.ic
  
  ## for convinience, pre-convert precision  to std deviation
  out$parms = as.data.frame(out$parms)
  sigma_add = 1/sqrt(out$parms$tau_add)
  
  ###### Forecast loop:
  print(paste("Forecasting for initial particle filter for site",
              as.character(site.number)))
  source("SSLPM.R")
  params = list()
  if(model == "LogitRandomWalk"){    
    for(t in 2:length(time)){
      X[t,] = pmax(0,pmin(1,
                          rnorm(num.ensembles,X[t-1,],sample(sigma_add,num.ensembles))))
    }
  } else if (model == "Threshold_Day_Logistic"){
    k = sample(out$parms$k,num.ensembles)
    r = sample(out$parms$r,num.ensembles)
    params$k = k
    params$r = r
    for(t in 2:length(time)){
      mu = ifelse(t>k,X[t-1,]-r*X[t-1,]*(1-X[t-1,]),1)
      X[t,] = pmax(0,pmin(1,
                          rnorm(num.ensembles,mu,sample(sigma_add,num.ensembles))))
    }
  } else{
    print(paste("Forecast for model not supported::",model))   
  }
  ##### end of forecast loop
  print("Initial forecasting complete.")
    
  # Date of the last data point used in the forecast:
  date.string <- paste(as.character(current.year),
                       format(as.Date(model.start.DOY-2,origin="2001-01-01"), 
                              format="%m-%d"),sep="-") 
  # Complicated! But just a date string to put in the file name. For the create FM 
  # model it's the day BEFORE the first day of data.
  
  ## Plot our forecast!
  X.ci  = apply(X,1,quantile,c(0.025,0.5,0.975))
    
  #### save plot produced to PDF    
  
  ## saves as PDF
  # Make a directory hierarchy to save pdfs in:
  dir.name <- paste("pdfs/site",as.character(site.number),sep="")
  dir.create(dir.name,recursive=TRUE,showWarnings=FALSE) # doesn't do anything if already created
  
  ## name of output file
  pdf_file_name = paste("ParticleFilterForecast",as.character(site.number),model,
                    as.character(date.string),"pdf",sep=".")
  
  pdf(file=paste(dir.name,pdf_file_name,sep="/"))
  
  ## plot forecast:
  plot(time,X.ci[2,],type='n',main=paste("Particle Filter Forecast:",date.string)
       ,xlab="Day of Year",ylab="Pheno-state",ylim=c(0,1.2))
  source("ciEnvelope.R")
  ciEnvelope(time,X.ci[1,],X.ci[3,],col="light grey")
  lines(time,X.ci[2,],main=paste("Particle Filter Forecast:",date.string)
        ,xlab="Day of Year",ylab="Pheno-state")
  
  ## ends plot output to PDF
  dev.off()

  source("ForecastThreshold.R")
  p = matrix(NA,nrow(X),5)
  png.file.name = paste("ThresholdForecast",as.character(site.number),model,
                      as.character(date.string),"png",sep=".")
  png(file=paste("png",png.file.name,sep="/"))
  p[1,] = ForecastThreshold(X)
  dev.off()
  
  ### save output (not sure if we want to save all of it... maybe just the most recent day's?)
  dir.create("forecastRData",recursive=TRUE,showWarnings=FALSE) # doesn't do anything if already created
  output_file_name = paste0("forecastRData/",paste("ForecastModel.X.out.site", as.character(site.number),model,date.string,
                                                 "RData",sep="."))
  save(X,params,p,file = output_file_name)

  ## name of initial ensemble forecast file
  print(sprintf("The particle filter forecast for site Num %.f is saved as %s",site.number,pdf_file_name))
  
  #### Save a file that contains the date of the last forecast:
  last.date.filename <- paste("last.update.site", as.character(site.number), model,
                              "txt",sep=".")
  sink(last.date.filename, append = FALSE)
  cat("\"",date.string,"\"",sep="")
  sink()  
}  
