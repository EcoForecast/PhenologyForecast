particle.filter.FM <- function(site_num){
  source("SSLPM.r") ## Super Simple Logistic Model
  source("ciEnvelope.R")
  source("global_input_parameters.R")
  source("update.r.R")  
  
  ########################################
  #### dummy values for debugging ########
  ########################################
  site_num=1
  
  ### See also ph.filter.sd below
  
  ### observation data (real thing should be cleaned, rescaled GCC and NDVI from 2013 only)
  obs=list()
  obs$GCC= c(rep(NA,182),cumsum(c(1,rnorm(344-182,-.005,.00001))))
  obs$NDVI= c(rep(NA,182),cumsum(c(1,rnorm(344-182,-.005,.00002))))
  obs$NDVI[210:240]=NA #
  obs$GCC[230:260]=NA #
  #########################################
  #########################################
  
  gcc.data <- read.csv( sprintf("gcc_data_site%i.csv",site_num) )
  gcc.2013 = gcc.data[gcc.data$date]
  
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
  
  ## Create a filter with GCC and NDVI equally weighted (uses only one data source if the other is NA)
  GCC = obs$GCC
  NDVI = obs$NDVI
  length = length(GCC)
  ph.filter=array(NA,(length))
  for(i in 1:length) {
    if (!is.na(GCC[i]) & !is.na(NDVI[i])) {
      val = mean(GCC[i], NDVI[i])
      ph.filter[i]=val
    } else if (!is.na(GCC[i]) & is.na(NDVI[i])) {
      ph.filter[i] = GCC[i] 
    } else if (is.na(NDVI[i]) & !is.na(NDVI[i])) {
      ph.filter[i] = NDVI[i] 
    }
  }
  
  ## just the fall dates
  ph.filter=ph.filter[time] 
  
  ## not sure how we will error for the particle weights, so I'm using 50% of the mean for now 
  ph.filter.sd = ph.filter*.5
  
  ### resampling particle filter
  sample=0
  hist.r=list()  ## since we resample parameters, create a record of what values were used each step
  hist.r[[1]] = r ## initial parameter conditions
  X = X.orig  ## reset state to the initial values, not the final values from the previous ensemble
  r = r.orig
  output = array(NA,c(nt,ne,2)) ## initialize output
  
  ###### here's the actual forecast loop
  for(t in 1:nt){
    
    ## forward step
    output[t,,]=as.matrix(SSLPM(X,r))
    X=output[t,,1]
    r=output[t,,2]
    
    ## analysis step
    #if(t%%(48*1) == 0){ ## if remainder == 0  ####### this peice is here as a template in case we don't filter every single day
    sample = sample+1
    print(sample)
    if(!is.na(ph.filter[sample])) {  ## if observation is present
      
      ## calulate Likelihood (weights)
      Lm = apply(output[t:1, ,1],2,mean) ## model filter over obs period
      wt = dnorm(ph.filter[sample],Lm,ph.filter.sd[sample])
      
      ## resample 
      index = sample.int(ne,ne,replace=TRUE,prob=wt)
      X = X[index]
      r = update.r(r,index)    
    }
    hist.r[[sample+1]] = r
    #}
  }
  ##### end of forecast loop
  
  ## Extract and summarize ph.filter
  ph.filter.pr = t(output[,,1])
  ph.filter.ci  = apply(ph.filter.pr,2,quantile,c(0.025,0.5,0.975))
  
  ##plot filter
  plot(time,ph.filter.ci[2,],type='n',ylab="NDVI_GCC",xlab="Time")
  ciEnvelope(time,ph.filter.ci[1,],ph.filter.ci[3,],col="light grey")
  points(time,ph.filter)    
  
}  