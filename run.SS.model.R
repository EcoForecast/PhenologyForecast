run.SS.model <- function(site_num){
  
  # source functions
  source("find.extreme.GCC.NDVI.R")
  source("RunJAGS.R")
  source("make.SS.plots.R")
  
  # load historical data, [data number, site_ID, date, NDVI, GCC], 
  # where site ID goes from 1 to 5
  hist_data = read.csv("full_historical_data.csv")
  
  # Get data from one site
  site_data = subset(hist_data,hist_data$site_ID == site_num)

  # find max/min of ndvi and gcc over all years of record except 2013
  # outputs (ndvi_max,ndvi_min,gcc_max,gcc_min)
  max_min_ndvi_gcc = find.extreme.GCC.NDVI(site_data, 2000, 2012, use.interannual.means=TRUE)  
  ndvi_max = max_min_ndvi_gcc[1]
  ndvi_min = max_min_ndvi_gcc[2]
  gcc_max = max_min_ndvi_gcc[3]
  gcc_min = max_min_ndvi_gcc[4]
  
  # Set parameters for linear data model 
  # Ignoring linear data model because we CAN'T let the x or color values go 
  # outside [0,1]. If they do (even because of uncertainty/error), then the
  # logistic model will keep it at 0 or 1 (just one of those.  Currently, model
  # gets stuck at 0).
  
#   beta0 <- ndvi_min/(ndvi_max-ndvi_min)
#   beta1 <- 1/(ndvi_max-ndvi_min)
#   beta2 <- gcc_min/(gcc_max-gcc_min)
#   beta3 <- 1/(gcc_max-gcc_min)
  
  # Set Spring Dates 1-181 to NA
  DAYS_TOT = c(1:366,rep(c(rep(1:365,3),1:366),3),1:365) 
  #Spring_dates = which(DAYS_TOT<182) # just keeping days 182-365
  Fall_dates = which(DAYS_TOT>=182)
  site_data = site_data[Fall_dates,]
  #site_data$NDVI[Spring_dates] = NA;
  #site_data$GCC[Spring_dates] = NA;
  
#   # If NDVI is negative, make NA
#   neg_NDVI = which(site_data$NDVI<0)
#   site_data$NDVI[neg_NDVI] = NA
  
  ## build data object for JAGS in year loop. 
  # pull out time, get year for each data point. 
  time=site_data$date
  time_year = as.numeric(format(as.Date(time), "%Y"))
  
  
  # Define the number of iterations and chains
  n.iter = 1000;
  n.chains = 3;
  
  # Stores output from each year
  # hardcode 13 because there is ALWAYS 13 years.
  # hardcoded 369 because there are always 369 output parameters [r  tau_add	tau_gcc	tau_ndvi	x]
  jags.out.all.years.array = array(rep(NA,n.iter*n.chains*188*13),
                                       c(n.iter*n.chains,188,13)) #369
  # counts for loop
  
  # Rescale data to be between 0 and 1 (using max and min NDVI, GCC values from 
  # all years):
  max_NDVI <- max(site_data$NDVI,na.rm=TRUE)
  min_NDVI <- min(site_data$NDVI,na.rm=TRUE)
  max_GCC <- max(site_data$GCC,na.rm=TRUE)
  min_GCC <- min(site_data$GCC,na.rm=TRUE)
  rescaled_NDVI <- (site_data$NDVI-min_NDVI)/(max_NDVI-min_NDVI)
  rescaled_GCC <- (site_data$GCC-min_GCC)/(max_GCC-min_GCC)
  
  count = 0
  for (YR in 2000:2012) {
    
    cat(sprintf("Running state-space model for site %i, year %i\n\n",site_num,YR))
    count = count + 1;
    # gets index of year
    II = which(time_year == YR)
    rescaled_NDVI_one_year = rescaled_NDVI[II]  # get ndvi just for ONE year
    rescaled_GCC_one_year = rescaled_GCC[II]    # get gcc just for ONE year

    # delete leap days 
    if (length(II) == 185){
      rescaled_NDVI_one_year = rescaled_NDVI_one_year[1:184] # [1:365]
      rescaled_GCC_one_year = rescaled_GCC_one_year[1:184] # [1:365]
    }
  
    # Make list "data" to be used as input for RunJAGS
    data <- list(y = rescaled_NDVI_one_year,z = rescaled_GCC_one_year,
                 n=length(rescaled_NDVI_one_year),x_ic=1,tau_ic=0.05,
                 a_ndvi=3.16,r_ndvi=.316,a_gcc=3.16,r_gcc=.316, #a_ndvi=.59,r_ndvi=1.69
                 a_add=1.41,r_add=.71)
     
    # run JAGS model 
    jags.out=RunJAGS(data,n.iter,n.chains)
    jags.out.matrix <- as.matrix(jags.out)
    jags.out.all.years.array[,,count] <- jags.out.matrix

#     source("ciEnvelope.R")
#     ci <- apply(( jags.out.matrix[,5:188]),2,quantile,c(0.025,0.5,0.975)) #[,5:369]
#     plot(182:365,ci[2,],type='l',ylim=c(0, 1),ylab="Rescaled NDVI, GCC",xlab="DOY")
#     ciEnvelope(182:365,ci[1,],ci[3,],col="lightBlue")
#     points(182:365,rescaled_NDVI_one_year,pch="+",cex=0.8)
#     points(182:365,rescaled_GCC_one_year,pch="o",cex=0.5)
#     lines(182:365,ci[2,],type='l',ylim=c(0, 1),ylab="Rescaled NDVI, GCC")
    
  }
  
  # Make filename and save jags output 
  file_name = paste('Jags.SS.out.site',as.character(site_num), 'RData',sep=".")
  save(jags.out.all.years.array, file = file_name)
  
  # Make plots
  make.SS.plots(jags.out.all.years.array,site_data$date,rescaled_NDVI,rescaled_GCC,site_num)
  
  
}