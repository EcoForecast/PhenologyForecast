initial.ensemble.FM <- function(site_num){
  source("SSLPM.r") ## Super Simple Logistic Model
  source("ciEnvelope.R")
  
  ########################################
  #### dummy values for debugging ########
  ########################################
  site_num=1

  
#########################################
#########################################

### read in output from State Space Model for X and r
file_name = paste('Jags.SS.out.site',as.character(site_num), 'RData',sep=".")
load(file_name)
X.from.SS = as.matrix(jags.out.all.years.array[,5,])
r.from.SS = as.matrix(jags.out.all.years.array[,1,])  

#initial values for each ensemble member (average of all years of historical data)
X.orig=apply(X.from.SS,1,mean)
r.orig=apply(X.from.SS,1,mean)
  
#take ensemble size from the size of the SS fit ensemble
ne=length(X.orig)
  
## Create vector for time to current date
cur_date = Sys.Date()
doy <- strftime(cur_date, format = "%j")
time = 182:doy
nt = length(time)

#### generate initial ensemble forecast
output = array(NA,c(nt,ne,2))
X = X.orig
r = r.orig
for(t in 1:nt){
  output[t,,]=as.matrix(SSLPM(X,r))
  X=output[t,,1]
  r=output[t,,2]
}

#plot mean and CI of ensemble members time series
varnames <- c("X","r")
for(i in 1:2){
  ci = apply(output[,,i],1,quantile,c(0.025,0.5,0.975))
  plot(time,ci[2,],,xlab="time",ylab=varnames[i],type='l')
  ciEnvelope(time,ci[1,],ci[3,],col="light blue")
  lines(ci[2,])
}

############## Sensitivity
############## Uncertainty
}
