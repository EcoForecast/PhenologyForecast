## forecast the date that a specific threshold is reached
ForecastThreshold <- function(X,thresh=0.5){
  
  p = numeric(nrow(X))
  for(i in 1:nrow(X)){
    d = density(X[i,],from=0,to=1,n=1001)
    j = findInterval(thresh,d$x)
    p[i] = d$y[j]
  }
  if(sum(p)>0){
    p = p/sum(p)
  }
  
  plot(182:365,p,xlab="DOY",type='l',lwd=3)
  pnts = wtd.quantile(182:365,p,c(0.025,0.25,0.5,0.75,0.975))
  abline(v=pnts,col=2,lty=c(3,2,1,2,3),lwd=2)
  
  return(pnts)
  
}

wtd.quantile <- function(x,wt,q){  ## weighted quantile
  ord <- order(x)
  wstar <- cumsum(wt[ord])/sum(wt)
  qi <- findInterval(q,wstar)
  qi[qi==0] = 1
  return(x[ord[qi]])
}