initial.ensemble.FM <- function(site_num,ne,inputs){
  
  ########################################
  #### dummy values for debugging ########
  ########################################
  site_num=1
  ne=10
  
  ### r parameter
  inputs=list()
  inputs$mean.r=c(0.05, 0.05, 0.05, 0.05, 0.05)
  inputs$sd.r=c(.0001, .0001, .0001, .0001, .0001)
  
  #########################################
  #########################################
  
  source("SSLPM.r") ## Super Simple Logistic Model
  source("ciEnvelope.R")
  
  ### Define Initial State
  if (ne > 1) { ## ne is number of ensemble members
    X = rbeta(ne,100,1) ## X is beta distributed very skewed to 1
  }
  X.orig = X
  
  ### Initial parameters (one for each ensemble member)
  r=rnorm(ne,inputs$mean.r[site_num],inputs$sd.r[site_num])
  
  ## Create vector for time to current date
  cur_date = Sys.Date()
  doy <- strftime(cur_date, format = "%j")
  time = 182:doy
  nt = length(time)
  
  #### generate initial ensemble forecast
  output = array(NA,c(nt,ne,2))
  for(t in 1:nt){
    output[t,,]=as.matrix(SSLPM(X,r))
    X=output[t,,1]
    r=output[t,,2]
  }
  
  #plot mean and CI of ensemble members time series
  var <- c("X","r")
  for(i in 1:2){
    ci = apply(output[,,i],1,quantile,c(0.025,0.5,0.975))
    plot(ci[2,],,xlab="time",ylab=var[i],type='l')
    ciEnvelope(1:nt,ci[1,],ci[3,],col="light blue")
    lines(ci[2,])
  }
  
  
  ############## Sensitivity
  ############## Uncertainty
}