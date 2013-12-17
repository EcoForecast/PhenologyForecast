## Super Simple Logistic Phenology Model
# This can obviously be switched out for a more complex model if desired.
## X is phenology state
## r is growth rate parameter in logistic equation
SSLPM <- function(X,r) { 
  Xnew = pmax(0,pmin(1,X-r*X*(1-X)))
  return(cbind(Xnew,r))
}