make.SS.plots <- function(jags.out.all.years.array,site_data){
  source("ciEnvelope.R")
  #pdf("output.pdf") #create output file
  count = 0
  
  time=site_data$date
  time_year = as.numeric(format(as.Date(time), "%Y"))
  
  for YR in 2000:2012{
    
    count = count+1
    jags.out.one.year = jags.out.all.years.array[,,count]

    II = which(time_year== YR)
    working_ndvi_yr = site_data$NDVI[II]  # get ndvi just for ONE year
    working_gcc_yr = site_data$GCC[II]    # get gcc just for ONE year
    
    # [r  tau_add  tau_gcc	tau_ndvi	x]
    ci <- apply((jags.out.one.year[,5:369]),2,quantile,c(0.025,0.5,0.975))
    
    # NDVI and GCC
    plot(1:365,ci[2,],type='l',ylim=c(0, 1),ylab="NDVI")
    ciEnvelope(1:365,ci[1,],ci[3,],col="lightBlue")
    points(1:365,working_ndvi_yr,pch="+",cex=0.8)
    points(1:365,working_gcc_yr,pch="o",cex=0.5)
    lines(1:365,ci[2,],type='l',ylim=c(0, 1),ylab="NDVI")
    
  }
  
  ## Find columns that are the state variable, x

}