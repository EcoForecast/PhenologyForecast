## Super Simple Stochastic Logistic Model
## X = [phenology]
## timestep is in seconds, defaults to 1 day
## X input going into SSSLM is going to be the previous state X estimate (i-1) (for loop from 2:length(timestep))
SSSLM <- function(X,params,inputs,timestep=1800) { 
  ne = nrow(X)  ## ne = number of ensemble members

  ## growth
  r ~ dnorm(0.5,110) ## growth rate for phenology model
  r_add=.71

  ## update states for phenology model
  Xnew = matrix(NA,ne,1)
  for (i in 2:length(X)) { 
  Xnew = max(0,min(1,X - r * X * (1-X) ))


return(data.frame(X1=Xnew))
}

## define CI Envelope function
  ciEnvelope <- function(x,ylo,yhi,...){
    polygon(cbind(c(x, rev(x), x[1]), c(ylo, rev(yhi),
                                        ylo[1])), border = NA,...) 
  }
  
## define colors function
  col.alpha <- function(col,alpha=1){
    rgb = col2rgb(col)
    rgb(rgb[1],rgb[2],rgb[3],alpha*255,maxColorValue=255)
  }

## weighted quantile
  wtd.quantile <- function(x,wt,q){ 
    ord <- order(x)
    wstar <- cumsum(wt[ord])/sum(wt)
    qi <- findInterval(q,wstar); qi[qi<1]=1;qi[qi>length(x)]=length(x)
    return(x[ord[qi]])
  }

## source("SetEnsembleSize.r")
  
## X.orig = SetEnsembleSize()
  
ne = 10 ## ensemble size
  
### Initial State (Mg/ha)
X = 1
if (ne > 1) {
    X = rnorm(ne,X,sd(0.0001))
  }
X.orig = X

##### here we use state space outputs to set priors for the particle filter

#### generate initial ensemble forecast
  nt = 800 ##length(time)
  ## time is our model driver
  ## get current date and day of year (between 182 to 365)
  cur_date = Sys.Date()
  doy <- strftime(cur_date, format = "%j")
  ## defines time vector as model driver
  time = 182:doy
  ## r is defined from state space output 
  output = array(NA,c(nt,ne,1))
  for(t in 1:nt){
    output[t,,]=as.matrix(SSSLM(X,r,time[t]))
    X=output[t,,]
  }
  


  
## read in state space x output
## read in historical data
## GCC = 
## NDVI = 
## filter with GCC and NDVI
## include if/else statement to constrain filter to use one data source if other data source has NA values
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
  lines(rep(Mtime[i],2),NDVI_GCC_filter[i]+c(-1,1)*LAIr.sd[i])
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
  
  plot(Mtime[Msel],LAIm.ci[2,],ylim=range(c(range(LAIm.ci),range(LAIr,na.rm=TRUE))),
       type='n',ylab="LAI",xlab="Time")
  ciEnvelope(Mtime[Msel],LAIm.ci[1,],LAIm.ci[3,],col=col.alpha("lightGrey",0.5))
  ciEnvelope(Mtime[Msel],LAIpf[1,],LAIpf[3,],col=col.alpha("lightBlue",0.5))
  ciEnvelope(Mtime[Msel],LAIpr.ci[1,],LAIpr.ci[3,],col=col.alpha("lightGreen",0.5))
  points(Mtime,LAIr)    
  for(i in 1:length(LAIr)){
    if(!is.na(QC[i])){
      lines(rep(Mtime[i],2),LAIr[i]+c(-1,1)*LAIr.sd[i])
    }
  }
  
  ### assess shifts in any parameter values
  par(mfrow=c(3,5))
  par(mar=c(2,2,4,0.7))
  for(i in 1:length(params)){
    if(is.null(dim(params[[i]]))){ ## parameter is scalar
      orig = density(hist.params[[1]][[i]])
      new = density(params[[i]])
      ylim=range(c(range(new$y),range(orig$y)))
      plot(orig,main=names(params)[i],xlab=" ",
           ylim=ylim)
      lines(new,col=2,lwd=2)
      text(max(orig$x),ylim[2],
           paste(format(mean(hist.params[[1]][[i]]),digits=3),
                 format(sd(hist.params[[1]][[i]]),digits=3)),
           pos=2)
      text(max(orig$x),ylim[2]*0.9,
           paste(format(mean(params[[i]]),digits=3),
                 format(sd(params[[i]]),digits=3)),
           pos=2)
    } else {
      ## parameter is vector
      for(j in 1:ncol(params[[i]])){
        orig = density(hist.params[[1]][[i]][,j])
        new = density(params[[i]][,j])
        ylim=range(c(range(new$y),range(orig$y)))
        plot(orig,main=paste(names(params)[i],j), xlab=" ",
             ylim=ylim)
        lines(new,col=2,lwd=2)
        text(max(orig$x),ylim[2],
             paste(format(mean(hist.params[[1]][[i]][,j]),digits=3),
                   format(sd(hist.params[[1]][[i]][,j]),digits=3)),
             pos=2)
        text(max(orig$x),ylim[2]*0.9,
             paste(format(mean(params[[i]][,j]),digits=3),
                   format(sd(params[[i]][,j]),digits=3)),
             pos=2)
      }      
    }  
  }
