SetEnsembleSize = function(ne,r,timestep) {
  #### inputs are ensemble size, growth rate and timestep
  ne = ne ## ensemble size
  
  ### Initial State (Mg/ha)
  X = 1
  if (ne > 1) {
    X = rnorm(ne,X,sd(0.0001))
  }
  X.orig = X
  
  return(X.orig)
}