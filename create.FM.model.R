create.FM.model <- function(site_num){
## source("SetEnsembleSize.r") ## if we write a function to make changing the ensemble size easy
ne = 10 ## ensemble size

### Define Initial State of ensemble members
if (ne > 1) {
  X = rbeta(ne,100,1) ## X is beta distributed very skewed to 1
}
X.orig = X

### read in output from State Space Model for X and r
file_name = paste('Jags.SS.out.site',as.character(site_num), 'RData',sep=".")
load(file_name)
### read in phenology data
X = as.matrix(jags.out.all.years.array[,5:369,])
r = as.vector(jags.out.all.years.array[,1,])
### read in r (growth rate parameter output)


##### for now while historical fit is still in progress:
## mean_r=.05
## sd_r=.0005

## Dummy data for testing
### Initial parameters (one for each ensemble member)
## r=rnorm(ne,mean_r,sd_r)

## Super Simple Logistic Phenology Model
## X is phenology state
## r is growth rate parameter in logistic equation
SSLPM <- function(X,r) { 
  ne = length(r)  ## ne = number of ensemble members
  # initialize new state
  Xnew = as.numeric(rep(NA,ne)) 
  # update state
  for(i in 1:ne){
  Xnew[i] = max(0,min(1,X[i] - r[i] * X[i] * (1-X[i]) ))
  }  
  return(data.frame(X=Xnew))
}

## Create vector for time to current date
cur_date = Sys.Date()
doy <- strftime(cur_date, format = "%j")
time = 182:doy
nt = length(time)

#### generate initial ensemble forecast
output = array(NA,c(nt,ne))
for(t in 1:nt){
  output[t,]=as.matrix(SSLPM(X,r))
  X=output[t,]
}

#plot mean and CI of ensemble members time series
source("ciEnvelope.R")
ci = apply(output,1,quantile,c(0.025,0.5,0.975))
plot(ci[2,],xlab="time",ylab="phenology",type='l')
ciEnvelope(1:nt,ci[1,],ci[3,],col="light blue")
lines(ci[2,])  



#### HERE THERE BE DRAGONS ###

## filter with GCC and NDVI (uses only one data source if other has all NA values)
for (i = 1:length(GCC)) {
  if (!is.na(GCC(i))) && (!is.na(NDVI(i))) {
    NDVI_GCC_filter(i) = 0.5*GCC(i) + 0.5*NDVI(i)}
  else if (!is.na(GCC(i))) && (is.na(NDVI(i))) {
    NDVI_GCC_filter(i)= GCC(i) }
  else if (is.na(NDVI(i))) && (!is.na(NDVI(i))) {
    NDVI_GCC_filter(i) = NDVI(i) }
}

## Calculate model ensemble means for same periods
window = rep(1,each=48*1,length=nt)
NDVI_GCC_m = t(apply(output[,,2],2,tapply,window,mean))
NDVI_GCC_m.ci  = apply(NDVI_GCC_m,2,quantile,c(0.025,0.5,0.975))

## plot model and observations
Msel = 1:ncol(NDVI_GCC_m.ci)
plot(Mtime[Msel],NDVI_GCC_m.ci[2,],ylab="NDVI_GCC",xlab="Time",
     ylim=range(c(range(NDVI_GCC_m.ci.ci),range(NDVI_GCC_filter,na.rm=TRUE))),type='n')
ciEnvelope(Mtime[Msel],NDVI_GCC_m.ci[1,],NDVI_GCC_m.ci[3,],col=col.alpha("lightGrey",0.5))
points(Mtime,NDVI_GCC_filter)    
for(i in 1:length(NDVI_GCC_filter)){
  lines(rep(Mtime[i],2),NDVI_GCC_filter[i]+c(-1,1)*NDVI_GCC_filter.sd[i])
}

### resampling particle filter
sample=0
hist.params=list()  ## since we resample parameters, create a record (history) of what values were used each step
hist.params[[1]] = r
X = X.orig  ## reset state to the initial values, not the final values from the previous ensemble
for(t in 1:nt){
  
  ## forward step
  output[t,,]=as.matrix(SSSLM(X,r,time[t]))
  X=output[t,,]
  
  ## analysis step
  ## changed analysis step to 48*1 to be 1/day
  if(t%%(48*1) == 0){ ## if remainder == 0
    sample = sample+1
    print(sample)
    if(!is.na(NDVI_GCC_filter[sample])){  ## if observation is present
      
      ## calulate Likelihood (weights)
      Lm = apply(output[t+1-(48*1):1, ,2],2,mean) ## model NDVI_GCC filter over obs period
      ## set std for filter to zero for now, make stochastic later
      NDVI_GCC_filter.sd = 0
      wt = dnorm(NDVI_GCC_filter[sample],Lm,NDVI_GCC_filter.sd)
      
      ## resample 
      index = sample.int(ne,ne,replace=TRUE,prob=wt)
      X = X[index]
      ### still need to define update.r
      r = update.r(r,index)    
    }
    hist.params[[sample+1]] = r
  }
  
}

## Extract and summarize NDVI_GCC FILTER (pr = resampling PF)
NDVI_GCC_filter.pr = t(apply(output[,,2],2,tapply,window,mean))
NDVI_GCC_filter.ci  = apply(NDVI_GCC_filter.pr,2,quantile,c(0.025,0.5,0.975))

plot(Mtime[Msel],NDVI_GCC_filter.ci[2,],ylim=range(c(range(LAIm.ci),range(NDVI_GCC_filter,na.rm=TRUE))),
     type='n',ylab="NDVI_GCC",xlab="Time")
ciEnvelope(Mtime[Msel],NDVI_GCC_m.ci[1,],NDVI_GCC_m.ci[3,],col=col.alpha("lightGrey",0.5))
ciEnvelope(Mtime[Msel],NDVI_GCC_filter.ci[1,],NDVI_GCC_filter.ci[3,],col=col.alpha("lightGreen",0.5))
points(Mtime,NDVI_GCC_filter)    
for(i in 1:length(NDVI_GCC_filter)){
  lines(rep(Mtime[i],2),NDVI_GCC_filter[i]+c(-1,1)*LAIr.sd[i])
}

