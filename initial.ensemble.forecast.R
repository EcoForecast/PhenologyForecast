initial.ensemble.FM <- function(site_num){
  source("SSLPM.r") ## Super Simple Logistic Model
  source("ciEnvelope.R")
  source("global_input_parameters.R")
  
  ########################################
  #### dummy values for debugging ########
  ########################################
  
  site_num=1
  
  #########################################
  #########################################
  
  ### read in output from State Space Model for X and r
  file_name = paste('Jags.SS.out.site',as.character(site_num), 'RData',sep=".")
  load(file_name)
  X.from.SS = as.matrix(jags.out.all.years.array[,5,]) # 5 is the first guess from X
  r.from.SS = as.matrix(jags.out.all.years.array[,1,])  
  
  # Remove state estimates that are < 0 and > 1 (we know they are WRONG)
  X.from.SS.NA = X.from.SS
  II = which(X.from.SS.NA<0 | X.from.SS.NA>1 )
  X.from.SS[II] = NA;
  r.from.SS[II] = NA;
  
  #initial values for each ensemble member (average of all years of historical data)
  X.orig=apply(X.from.SS,1,mean,na.rm=TRUE)
  r.orig=apply(r.from.SS,1,mean,na.rm=TRUE)
  
  #take ensemble size from the size of the SS fit ensemble
  ne = 1000 # length(X.orig)
  
  # Pulls from a distrubtion... need to work on this
  r.orig.dist = sample(r.orig,size=ne)
  X.orig.dist = sample(X.orig,size=ne)
  
  ## Create vector for time to current date
#   cur_date = Sys.Date()
#   doy <- strftime(cur_date, format = "%j")
#   time = global_input_parameters$model.start.DOY:doy
  time = global_input_parameters$model.start.DOY:365 # Changed so it predicts the whole year
  nt = length(time)
  
  #### generate initial ensemble forecast
  output = array(NA,c(nt,ne,2))
  X = X.orig.dist
  r = r.orig.dist

  for(t in 1:nt){
    output[t,,]=as.matrix(SSLPM(X,r))
    X=output[t,,1]
    r=output[t,,2]
  }
  
#### save plots produced to PDF
  ## name of output file
  file_name = paste('initial.ensemble.forecast',as.character(site_num), 'pdf',sep=".")
  ## saves as PDF
  pdf(file=file_name)
  
  #plot mean and CI of ensemble members time series
  ci = apply(output[,,1],1,quantile,c(0.025,0.5,0.975),na.rm=TRUE)
  plot(time,ci[2,],ylim=c(0,1),xlab="time",ylab="X",type='l')
  ciEnvelope(time,ci[1,],ci[3,],col="light blue")
  lines(time,ci[2,])
  
  ci = apply(output[,,2],1,quantile,c(0.025,0.5,0.975),na.rm=TRUE)
  plot(time,ci[2,],ylim=c(-1,1),xlab="time",ylab="r",type='l')
  ciEnvelope(time,ci[1,],ci[3,],col="light blue")
  lines(time,ci[2,])
  
  ############## Uncertainty: plot ensemble forecast and observed data so far.
  # Get data from one site and one year
  hist_data = read.csv("full_historical_data.csv")
  site_data = subset(hist_data,hist_data$site_ID == site_num)
  time2=site_data$date
  time_year = as.numeric(format(as.Date(time2), "%Y"))

  max_NDVI <- max(site_data$NDVI,na.rm=TRUE)
  min_NDVI <- min(site_data$NDVI,na.rm=TRUE)
  max_GCC <- max(site_data$GCC,na.rm=TRUE)
  min_GCC <- min(site_data$GCC,na.rm=TRUE)
  rescaled_NDVI <- (site_data$NDVI-min_NDVI)/(max_NDVI-min_NDVI)
  rescaled_GCC <- (site_data$GCC-min_GCC)/(max_GCC-min_GCC)
  
  II = which(time_year==2013)
  gcc2013 = rescaled_GCC[II]
  ndvi2013 = rescaled_NDVI[II]
  
  ci = apply(output[,,1],1,quantile,c(0.025,0.5,0.975),na.rm=TRUE)
  plot(time,ci[2,],ylim=c(0,1),xlab="time",ylab="X",type='l')
  ciEnvelope(time,ci[1,],ci[3,],col="light blue")
  points(global_input_parameters$model.start.DOY:365,gcc2013[global_input_parameters$model.start.DOY:365],pch="+",cex=0.8)
  points(global_input_parameters$model.start.DOY:365,ndvi2013[global_input_parameters$model.start.DOY:365],pch="o",cex=0.8)
  lines(time,ci[2,])
  
## ends plot output to PDF
  dev.off()
  
## name of initial ensemble forecast file
  sprintf('The initial ensemble forecast for site No %.f is saved as %s',site_num,file_name)

  ############## Sensitivity
  # Since there is no environmental/climate driver, what are we taking the sensitivity of?
  
}
