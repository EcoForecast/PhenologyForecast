## Super Simple Logistic Phenology Model
## X is phenology state
## r is growth rate parameter in logistic equation
SSLPM <- function(X,r) { 
  ne = length(X)  ## ne = number of ensemble members
  # initialize new state
  Xnew = as.numeric(rep(NA,ne)) 
  # update state
  for(i in 1:ne){
    Xnew[i] = max(0,min(1,X[i] - r[i] * X[i] * (1-X[i]) ))
  }  
  return(data.frame(X=Xnew,r=r))
}