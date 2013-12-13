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
  ne=length(X.orig)
  
  # Pulls from a distrubtion... need to work on this
  X.orig.dist = rnorm(ne,X.orig,sd(X.from.SS,na.rm=TRUE)) 
  r.orig.dist = rnorm(ne,r.orig,sd(r.from.SS,na.rm=TRUE))
  
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
  
  #plot mean and CI of ensemble members time series
  ci = apply(output[,,1],1,quantile,c(0.025,0.5,0.975),na.rm=TRUE)
  plot(time,ci[2,],ylim=c(0,1),xlab="time",ylab="X",type='l')
  ciEnvelope(time,ci[1,],ci[3,],col="light blue")
  lines(time,ci[2,])
  
  ci = apply(output[,,2],1,quantile,c(0.025,0.5,0.975),na.rm=TRUE)
  plot(time,ci[2,],ylim=c(-1,1),xlab="time",ylab="r",type='l')
  ciEnvelope(time,ci[1,],ci[3,],col="light blue")
  lines(time,ci[2,])
  
  ############## Sensitivity
  ############## Uncertainty
  
}
