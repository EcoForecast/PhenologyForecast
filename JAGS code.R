## this code needs:
#### are GCC and NDVI in the same units/normalized correctly in the full.historical.data.csv?
#### values for priors in data object, and for r in the JAGS code need to be figured out better: based on literature, not guesses
#### Make the jags output save into seperate files for each site. Right now it loops through the 5 sites but overwrites "jags.out" at each loop.
#### Need to make diagnostic plots

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

## run jags model
## define number of iterations here
jags.out=RunJAGS(data,n.iter=100)

### how to make this be 5 different files?
output[[j]] <- jags.out
}
