### define function that runs JAGS model
RunJAGS <- function(data,n.iter,n.chains){
  require(rjags)
  
  ##JAGS code
  ModisGCCModel = "
  model{
  #### Data Model: NDVI
  for(i in 1:n){
  y[i] ~ dnorm(x[i],tau_ndvi)
  }
  
  #### Data Model: GCC
  for(i in 1:n){
  z[i] ~ dnorm(x[i],tau_gcc)
  }
  
  #### Process Model
  #### Color is the expected new phenology stage given the previous stage and logistic 
  #### subtraction instead of addition in the discrete logistic eqn makes r negative (so logistic goes down).
  for(i in 2:n){
  color[i] <- max(0, min(1, x[i-1] - r * x[i-1] * (1-x[i-1]) ) )
  x[i]~dnorm(color[i],tau_add)
  }
  
  #### Priors
  x[1] ~ dnorm(x_ic,tau_ic)
  tau_ndvi ~ dgamma(a_ndvi,r_ndvi)
  tau_gcc ~ dgamma(a_gcc,r_gcc)
  tau_add ~ dgamma(a_add,r_add)
  r ~ dnorm(0.5,1)

  }"
    
  ## JAGS initial conditions
  init <- list()
  for(i in 1:n.chains){
    y.samp = sample(data$y,length(data$y),replace=TRUE)
    ########## what are the values for tau_ndvi and tau_gcc based on? is this reasonable?
    init[[i]] <- list(x = rep(1,length(data$y)), 
                      tau_add = runif(1,0,1)/var(diff(y.samp),na.rm=TRUE),
                      tau_ndvi = 10,tau_gcc=10)
  }
  
  ## compile JAGS model
  j.model   <- jags.model (file = textConnection(ModisGCCModel),
                           data = data,
                           inits = init,
                           n.chains = n.chains)
  ## burn-in
  jags.out   <- coda.samples (model = j.model,
                              variable.names = c("tau_add","tau_ndvi","tau_gcc","r"),
                              n.iter = min(n.iter,2000))
  
  ## run MCMC
  jags.out   <- coda.samples (model = j.model,
                              variable.names = c("x","tau_add","tau_ndvi","tau_gcc","r"),
                              n.iter = n.iter)
  return(jags.out)
}