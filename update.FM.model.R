update.FM.model <- function(site.number) {
  # The function update.FM.model updates an already existing particle filter
  # forecast model. It first checks for new data, then assimilates that data into
  # that forecast using a resampling particle filter. Outputs are generated one day
  # at a time. Days with no new data are ignored, and the previous forecast values 
  # for that day are used instead. The function stops when all observed data has
  # been assimilated. 
  # The forecast for each day is plotted and saved to a pdf begining with 
  # ParticleFilterForecast (with a site number and date appended). The output from 
  # the current forecast is saved in a file begining with ForecastModel.X.out (with 
  # a site number and date appended).
  
  source("SSLPM.R")
  source("ciEnvelope.R")
  source("find.extreme.GCC.NDVI.R")
  
  current.year <- strftime(Sys.Date(),"%Y")
  source("global_input_parameters.R")
  model.start.DOY <- global_input_parameters$model.start.DOY
  
  ##### get the date of the last forecast:
  last.date.filename <- paste("last.update.site", as.character(site.number), 
                              "txt",sep=".")
  read.in <- source(last.date.filename)
  last.forecast.date <- as.Date(read.in$value)
  last.date.assimilated <- last.forecast.date
  
  # load the GCC data:
  gcc.data <- read.csv( sprintf("gcc_data_site%i.csv",site.number) )
  
  # load the NDVI data:
  ndvi.data <- read.csv( sprintf("ndvi_data_site%i.csv",site.number) )
  
  # Merge them:
  all.data <- merge(gcc.data,ndvi.data)
  
  ##### Rescale the data:
  # find max/min of ndvi and gcc over all years of record except current
  # outputs (ndvi_max,ndvi_min,gcc_max,gcc_min)
  first.year <- as.numeric(strftime(global_input_parameters$data.start.date, "%Y"))
  
  max_min_ndvi_gcc = find.extreme.GCC.NDVI(site.number, first.year, 
                                           as.numeric(current.year)-1, 
                                           use.interannual.means=TRUE)
  ndvi_max = max_min_ndvi_gcc[1]
  ndvi_min = max_min_ndvi_gcc[2]
  gcc_max = max_min_ndvi_gcc[3]
  gcc_min = max_min_ndvi_gcc[4]
  # Rescale data to be between 0 and 1 (using max and min NDVI, GCC values from 
  # all years except current year):
  # rescale NDVI (and overwrite all.data$ndvi!)
  all.data$ndvi <- (all.data$ndvi-ndvi_min)/(ndvi_max-ndvi_min)
  # rescale GCC:
  all.data$gcc.90 <- (all.data$gcc.90 - gcc_min)/(gcc_max - gcc_min)
  all.data$gcc.mean <- (all.data$gcc.mean - gcc_min)/(gcc_max - gcc_min)
  all.data$gcc.min <- (all.data$gcc.min - gcc_min)/(gcc_max - gcc_min)
  all.data$gcc.max <- (all.data$gcc.max - gcc_min)/(gcc_max - gcc_min)  
  
  # load the forecast model output:
  output_file_name = paste0("forecastRData/",paste("ForecastModel.X.out.site", as.character(site.number),last.forecast.date, 
                           "RData",sep="."))
  load(output_file_name)
  
  # Number of ensemble members:
  num.ensemble <- global_input_parameters$num.ensembles
  
  forecast.date <- last.forecast.date + 1
  
  current.date <- Sys.Date()
  
  # Get standard deviations for measurement error from tau_gcc and tau_ndvi from
  # our state-space model
  file_name = paste('Jags.SS.out.site',as.character(site.number), 'RData',sep=".")
  load(file_name)
  out$parms = as.data.frame(out$parms)
  
  # get the precisions from the state space model output, convert to stdevs:
  gcc.stdev <- 1/sqrt(out$parms$tau_gcc)
  ndvi.stdev <- 1/sqrt(out$parms$tau_ndvi)
  proc.stdev <- 1/sqrt(out$parms$tau_add)  
  
  ## for now, lets work with the median value for all std deviations
  ## will look into accomodating their uncertainty in the future
  gcc.stdev  = median(gcc.stdev)
  ndvi.stdev = median(ndvi.stdev)
  proc.stdev = median(proc.stdev)
  
  # while loop until you get to the present day:
  repeat{
    # Keep this break statement floating at the top of the repeat loop:
    if(forecast.date > current.date) {break} # This will end the loop
    
    print(paste("Running particle filter for",forecast.date,"at site",site.number))
    todays.data <- all.data[as.Date(all.data$date) == forecast.date,]
    new.data <- !(is.na(todays.data$gcc.90) & is.na(todays.data$ndvi)) # TRUE/FALSE
    
    # Only need to do anything when there is new data
    if(new.data) {
      
      # Let's get today's incoming X values:
      output.days <- nrow(X)
      output.index <- output.days - as.numeric(as.Date(paste(current.year,"12-31",sep="-")) - forecast.date,
                                               unit="days")     
      Xf = X[output.index,]
      
      #### Analysis step:
      # Calculate the likelihood of our ensemble members given the data:
      if(is.na(todays.data$ndvi)){
        log.likelihood.ndvi <- rep(0,num.ensemble) # no likelihood if no data...
      } else {
        log.likelihood.ndvi <- dnorm(Xf,todays.data$ndvi,ndvi.stdev,log=TRUE)
      }
      if(is.na(todays.data$gcc.90)){
        log.likelihood.gcc <- rep(0,num.ensemble) # no likelihood if no data...
      } else {
        log.likelihood.gcc <- dnorm(Xf,todays.data$gcc.mean,gcc.stdev,log=TRUE)
      }
      likelihood <- exp(log.likelihood.gcc + log.likelihood.ndvi)
      
      # if there is an outlier, so bad that it crashed the model, we set 
      # the likelihoods to all the same (smallish) value
      if (sum(likelihood)==0){    
        likelihood = rep(0.00001,length(likelihood))
      }
      
      #### Resampling step:
      index = sample.int(num.ensemble, num.ensemble, replace = TRUE, prob = likelihood)
      # replace our previous guess with the PF output:
      X[output.index,] = X[output.index,index] #pmin(1,pmax(0,X[output.index,index]))
      
      #### Forecast step:
      # as long as we're not at the end of the year:
      if(forecast.date < as.Date(paste(current.year,"12-31",sep="-"))) {
        # Forecast!
        if(global_input_parameters$model == "LogitRandomWalk"){
          for(t in (output.index+1):output.days){
                X[t,] = pmax(0,pmin(1,rnorm(num.ensemble,X[t-1,],proc.stdev)))
#                X[t,] = rnorm(num.ensemble,X[t-1,],proc.stdev)
            }
        } else {
            print(paste("Forecast for model not supported::",global_input_parameters$model))   
        }
      }        

      ##### end of forecast loop
      
      # Plot the forecast!      
      X.ci  = apply(X,1,quantile,c(0.025,0.5,0.975))
      
      #### save plot produced to PDF
      ## name of output file
      dir.name <- paste("pdfs/site",as.character(site.number),sep="") 

      ## name of output file
      pdf.file.name = paste("ParticleFilterForecast",as.character(site.number),
                            as.character(forecast.date),"pdf",sep=".")
      
      
      ## saves as PDF
      pdf(file=paste(dir.name,pdf.file.name,sep="/"))
      
      #### plot forecast:
      # get rid of data from the future!
      plottable.data <- subset(all.data,as.Date(all.data$date) <= forecast.date)
      # get rid of data from previous years:
      plottable.data <- subset(plottable.data, 
                               strftime(as.Date(plottable.data$date),"%Y") == current.year)
      # Get rid of early part of year:
      plottable.data <- subset(plottable.data,
                               as.Date(plottable.data$date) >= model.start.DOY)
      
      plot(model.start.DOY:365,X.ci[2,],type='n',
           main=paste("Particle Filter Forecast:",forecast.date),
           xlab="Day of Year",ylab="Pheno-state",ylim=c(0,1.2))
      ciEnvelope(model.start.DOY:365,X.ci[1,],X.ci[3,],col="light grey")
      lines(model.start.DOY:365,X.ci[2,],
            main=paste("Particle Filter Forecast:",forecast.date),
            xlab="Day of Year",ylab="Pheno-state")
      
      non.leap.year.doys <- as.numeric(strftime(plottable.data$date,"%j")) - (as.numeric(current.year)%%4 == 0)
      points(non.leap.year.doys, plottable.data$ndvi, pch="+",cex=0.8)
      points(non.leap.year.doys, plottable.data$gcc.mean, pch="o",cex=0.5)
      
      
      ## ends plot output to PDF
      dev.off()

      ## also output in png for the webpage
    png.file.name = paste("ParticleFilterForecast",as.character(site.number),
                      as.character(forecast.date),"png",sep=".")
    png(file=paste("png",png.file.name,sep="/"))

    plot(model.start.DOY:365,X.ci[2,],type='n',
       main=paste("Particle Filter Forecast:",forecast.date),
       xlab="Day of Year",ylab="Pheno-state",ylim=c(0,1.2))
    ciEnvelope(model.start.DOY:365,X.ci[1,],X.ci[3,],col="light grey")
    lines(model.start.DOY:365,X.ci[2,],
      main=paste("Particle Filter Forecast:",forecast.date),
      xlab="Day of Year",ylab="Pheno-state")
    points(non.leap.year.doys, plottable.data$ndvi, pch="+",cex=0.8)
    points(non.leap.year.doys, plottable.data$gcc.mean, pch="o",cex=0.5)

    dev.off()
              
      #### append output to pdf files that were created in the forecast model:

      # Save the most recent output data to file:
      output_file_name = paste0("forecastRData/",paste("ForecastModel.X.out.site", as.character(site.number),forecast.date,
                         "RData",sep="."))
      save(X,file=output_file_name)   

      # Write the last forecast date to file:
      date.string <- as.character(last.date.assimilated)
      last.date.filename <- paste("last.update.site", as.character(site.number), 
                            "txt",sep=".")
      sink(last.date.filename, append = FALSE)
      cat("\"",date.string,"\"",sep="")
      sink()  
      
      # This is important as it is the date to save in the file tracking the last 
      # date assimilated
      last.date.assimilated <- forecast.date
      
    } # end if(new.data)
    
    # Increment the date, and update again!
    forecast.date <- forecast.date + 1    
  }
  

  
}
