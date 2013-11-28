## add MCMC diagnostics, check distributions and priors

# load data frame with phenocam and modis data
### this assumes that NDVi and GCC are already on a 0 to 1 scale.
hist_data = read.csv("full_historical_data.csv")

# loop over the 5 sites, running the JAGS model on each one.
SITE_NAME = c("Coweeta","Shalehillsczo","Howland","Shenandoah","Bartlett")

# initiate output list to populate with output
output = list()

for(j in 1:5){
  ## pull out NDVI
  ndvi=hist_data$NDVI[hist_data$site_ID==j]
  
  ## pull out GCC
  gcc=hist_data$GCC[hist_data$site_ID==j] 
  
  # pull out time
  time=hist_data$date[hist_data$site_ID==j] 
  
  # Extract Fall Date 182-365
  VEC1 = c(1:366,rep(c(rep(1:365,3),1:366),3),1:365)
  Spring_dates = which(VEC1<181)
  
  ndvi[Spring_dates] = NA
  gcc[Spring_dates] = NA
  
  # If NDVI is negative, make NA
  neg_NDVI = which(ndvi<0)
  ndvi[neg_NDVI] = NA
}

### define function that runs JAGS model
RunJAGS <- function(data,n.iter){
  require(rjags)
  
  ## build data object for JAGS
  ######## what should these priors be????
  y=ndvi
  z=gcc
  data <- list(y = ndvi,z = gcc,n=length(y),x_ic=1,tau_ic=0.000001,
               a_ndvi=2,r_ndvi=2,a_gcc=2,r_gcc=2,a_add=2,r_add=2)
  
  ##JAGS code
  ModisGCCModel = "
  model{
  #### Data Model: NDVI
  for(i in 1:n){
  y[i] ~ dnorm(x[i],tau_ndvi)
  }
  
  #### Data Model: GCC
  for(i in 2:n){
  z[i] ~ dnorm(x[i],tau_gcc)
  }
  
  #### Process Model
  #### Color is the expected new phenology stage given the previous stage and logistic 
  #### subtraction instead of addition in the discrete logistic eqn makes r negative (so logistic goes down).
  for(i in 2:n){
  color[i] <- x[i-1] - r * x[i-1] * (1-x[i-1]) 
  x[i]~dnorm(color[i],tau_add)
  }
  
  #### Priors
  x[1] ~ dnorm(x_ic,tau_ic)
  tau_ndvi ~ dgamma(a_ndvi,r_ndvi)
  tau_gcc ~ dgamma(a_gcc,r_gcc)
  tau_add ~ dgamma(a_add,r_add)
  r ~ dnorm(0.5,0.5)
  }"
    
  ## JAGS initial conditions
  nchain = 3
  init <- list()
  for(i in 1:nchain){
    y.samp = sample(y,length(y),replace=TRUE)
    ########## what are the values for tau_ndvi and tau_gcc based on? is this reasonable?
    init[[i]] <- list(x = rep(1,length(y)), tau_add=runif(1,0,1)/var(diff(y.samp),na.rm=TRUE),
                      tau_ndvi=10,tau_gcc=10)
  }
  
  ## compile JAGS model
  j.model   <- jags.model (file = textConnection(ModisGCCModel),
                           data = data,
                           inits = init,
                           n.chains = 3)
  ## burn-in
  jags.out   <- coda.samples (model = j.model,
                              variable.names = c("tau_add","tau_ndvi","tau_gcc","r"),
                              n.iter = min(n.iter,2000))
  #plot(jags.out)
  
  ## run MCMC
  jags.out   <- coda.samples (model = j.model,
                              variable.names = c("x","tau_add","tau_ndvi","tau_gcc","r"),
                              n.iter = n.iter)
  return(jags.out)
}

## defines confidence interval function
ciEnvelope <- function(x,ylo,yhi,...){
  polygon(cbind(c(x, rev(x), x[1]), c(ylo, rev(yhi),
                                      ylo[1])), border = NA,...) 
}

for(j in 1:5){
  ## run jags model
  ## define number of iterations here
  jags.out=RunJAGS(data,n.iter=100)
  
  ### saves output from each site into a list element
  output[[j]] <- jags.out
  
  # Creates plots for output and diagnostics
  ## turns output into matrix
  out <- as.matrix(jags.out)
  x.cols = which(substr(colnames(out),1,1)=="x")   ## which columns are the state variable, x
  ci <- apply((out[,x.cols]),2,quantile,c(0.025,0.5,0.975))
  
  # NDVI
  par(mfrow=c(1,1))
  plot(time,ci[2,],type='l',ylim=c(0, 5),ylab="NDVI")
  ciEnvelope(time,ci[1,],ci[3,],col="lightBlue")
  points(time,ndvi,pch="+",cex=1.5)
  
  # GCC
  par(mfrow=c(1,1))
  plot(time,ci[2,],type='l',ylim=c(0, 5),ylab="gcc")
  ciEnvelope(time,ci[1,],ci[3,],col="lightBlue")
  points(time,gcc,pch="+",cex=1.5)
  
  #plot.new()
  par(mfrow=c(2,2))
  
  # tau_NDVI
  # convert from precision to std
  tau_ndvi = 1/sqrt(out[,4])
  hist(tau_ndvi,main = colnames(out)[4])
  
  # tau_GCC
  # convert from precision to std
  tau_gcc = 1/sqrt(out[,3])
  hist(tau_gcc,main = colnames(out)[3])
  
  # tau_add
  # convert from precision to std
  tau_add = 1/sqrt(out[,2])
  hist(tau_add,main = colnames(out)[2])
  
  cor1 = cor(out[,2:4])
  sprintf('The correlation coefficient between tau_add and tau_gcc is %f',cor1[2])
  sprintf('The correlation coefficient between tau_add and tau_ndvi is %f',cor1[3])
  sprintf('The correlation coefficient between tau_gcc and tau_ndvi is %f',cor1[6])
  
}  


# cor(out[,1:2])