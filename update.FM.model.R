update.FM.model <- function(site_num) {
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
  
  site_num = 1
  
  current.year <- strftime(Sys.Date(),"%Y")
  source("global_input_parameters.R")
  model.start.DOY <- global_input_parameters$model.start.DOY
  
  ##### get the date of the last forecast:
  last.date.filename <- paste("last.update.site", as.character(site_num), 
                              "txt",sep=".")
  read.in <- source(last.date.filename)
  last.forecast.date <- as.Date(read.in$value)
  last.date.assimilated <- last.forecast.date
  
  # load the GCC data:
  gcc.data <- read.csv( sprintf("gcc_data_site%i.csv",site_num) )
  
  # load the NDVI data:
  ndvi.data <- read.csv( sprintf("ndvi_data_site%i.csv",site_num) )
  
  # Merge them:
  all.data <- merge(gcc.data,ndvi.data)
  
  # load the forecast model output:
  output_file_name = paste("ForecastModel.X.out.site", as.character(site_num),
                           "RData",sep=".")
  load(output_file_name) # loads a num_days x num_ensemble x 2 array called output
  
  forecast.date <- last.forecast.date + 1
  
  current.date <- Sys.Date()
  
#   ##########################
#   ##########################
#   # This stdev stuff should go into "output" in the FM creation step so that 
#   # the samples stay together properly!
#   ##########################
#   ##########################
#   
#   # Get standard deviations for measurement error from tau_gcc and tau_ndvi from
#   # our state-space model for now?
#   file_name = paste('Jags.SS.out.site',as.character(site_num), 'RData',sep=".")
#   load(file_name)
#   gcc.stdev <- apply(jags.out.all.years.array[,3,],1,mean) # num.ensemble members x 1
#   ndvi.stdev <- apply(jags.out.all.years.array[,4,],1,mean) # num.ensemble members x 1
  
  # Obviously not a long-term solution...
  gcc.stdev <- 0.4
  ndvi.stdev <- 0.4
  
  # while loop until you get to the present day:
  repeat{
    # Keep this break statement floating at the top of the repeat loop:
    if(forecast.date > current.date) {break} # This will end the loop
    
    todays.data <- all.data[as.Date(all.data$date) == forecast.date,]
    new.data <- !(is.na(todays.data$gcc.90) & is.na(todays.data$ndvi)) # TRUE/FALSE
    
    # Only need to do anything when there is new data
    if(new.data) {
      
      # Let's get today's incoming X and r values:
      output.days <- dim(output)[1]
      output.index <- output.days - as.numeric(as.Date(paste(current.year,"12-31",sep="-")) - forecast.date,
                                               unit="days")
      X <- output[output.index,,1] # vector
      r <- output[output.index,,2] # vector
      
      #### Analysis step:
      # Calculate the likelihood of our ensemble members given the data:
      if(is.na(todays.data$ndvi)){
        likelihood.ndvi <- rep(0,3000) # no likelihood if no data...
      } else {
        likelihood.ndvi <- dnorm(X,todays.data$ndvi,ndvi.stdev)
      }
      if(is.na(todays.data$gcc.90)){
        likelihood.gcc <- rep(0,3000) # no likelihood if no data...
      } else {
        likelihood.gcc <- dnorm(X,todays.data$gcc.90,gcc.stdev)
      }
      likelihood <- likelihood.gcc + likelihood.ndvi
      
      #### Resampling step:
      index = sample.int(length(X), length(X), replace = TRUE, prob = likelihood)
      # replace our previous guess with the PF output:
      output[output.index,,1] = X[index]
      output[output.index,,2] = r[index] 
      
      #### Forecast step:
      # as long as we're not at the end of the year:
      if(forecast.date < as.Date(paste(current.year,"12-31",sep="-"))) {
        # Forecast!
        for(t in (output.index+1):output.days){
          X = output[t-1,,1]
          r = output[t-1,,2]
          ## forward step
          output[t,,] = SSLPM(X,r) # num.ensembles x 2
        }        
      }
      ##### end of forecast loop
      
      # Plot the forecast!      
      X.mat = output[,,1]
      X.ci  = apply(X.mat,1,quantile,c(0.025,0.5,0.975))
      
      #### save plot produced to PDF
      ## name of output file
      pdf.file.name = paste("ParticleFilterForecast",as.character(site_num),
                        as.character(forecast.date),"pdf",sep=".")
            
      ## saves as PDF
      pdf(file=pdf.file.name)
      
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
           xlab="Day of Year",ylab="Pheno-state")
      ciEnvelope(model.start.DOY:365,X.ci[1,],X.ci[3,],col="light grey")
      lines(model.start.DOY:365,X.ci[2,],
            main=paste("Particle Filter Forecast:",forecast.date),
            xlab="Day of Year",ylab="Pheno-state")

      non.leap.year.doys <- as.numeric(strftime(plottable.data$date,"%j")) - (as.numeric(current.year)%%4 == 0)
      points(non.leap.year.doys, plottable.data$ndvi, pch="+",cex=0.8)
      points(non.leap.year.doys, plottable.data$gcc.90, pch="o",cex=0.5)
      
      
      ## ends plot output to PDF
      dev.off()
      
      
      
      #### append output to pdf files that were created in the forecast model:
      
      
      # This is important as it is the date to save in the file tracking the last 
      # date assimilated
      last.date.assimilated <- forecast.date
        
    } # end if(new.data)
    
    # Increment the date, and update again!
    forecast.date <- forecast.date + 1    
  }
  
  # Save the most recent output data to file:
  
  
  
  
  # Write the last forecast date to file:
  date.string <- as.character(last.date.assimilated)
  last.date.filename <- paste("last.update.site", as.character(site_num), 
                              "txt",sep=".")
  sink(last.date.filename, append = FALSE)
  cat("\"",date.string,"\"",sep="")
  sink()  
   
}
  