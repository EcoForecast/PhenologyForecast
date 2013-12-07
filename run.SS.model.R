run.SS.model <- function(site_num){
  
  site_num = 1
  # source functions
  source("find.extreme.GCC.NDVI.R")
  source("RunJAGS.R")
  
  # load historical data, [data number, site_ID, date, NDVI, GCC], 
  # where site ID goes from 1 to 5
  hist_data = read.csv("full_historical_data.csv")
  
  # Get data from one site
  site_data = subset(hist_data,hist_data$site_ID == site_num)

  # find max/min of ndvi and gcc over all years of record except 2013
  # outputs (ndvi_max,ndvi_min,gcc_max,gcc_min)
  max_min_ndvi_gcc = find.extreme.GCC.NDVI(site_data)  
  ndvi_max = max_min_ndvi_gcc[1]
  ndvi_min = max_min_ndvi_gcc[2]
  gcc_max = max_min_ndvi_gcc[3]
  gcc_min = max_min_ndvi_gcc[4]
  
  # Set parameters for linear data model 
  beta0 <- ndvi_min/(ndvi_max-ndvi_min)
  beta1 <- 1/(ndvi_max-ndvi_min)
  beta2 <- gcc_min/(gcc_max-gcc_min)
  beta3 <- 1/(gcc_max-gcc_min)
  
  # Set Spring Dates 1-181 to NA
  DAYS_TOT = c(1:366,rep(c(rep(1:365,3),1:366),3),1:365) 
  Spring_dates = which(DAYS_TOT<182) # just keeping days 182-365
  site_data$NDVI[Spring_dates] = NA;
  site_data$GCC[Spring_dates] = NA;
  
  # If NDVI is negative, make NA
  neg_NDVI = which(site_data$NDVI<0)
  site_data$NDVI[neg_NDVI] = NA
  
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
  jags.out.all.years.array = array(rep(NA,n.iter*n.chains*369*13),c(n.iter*n.chains,369,13)) 
  # counts for loop
  count = 0
  for (YR in 2010:2010) {
    
    count = count + 1;
    # gets index of year
    II = which(time_year== YR)
    working_ndvi_yr = site_data$NDVI[II]  # get ndvi just for ONE year
    working_gcc_yr = site_data$GCC[II]    # get gcc just for ONE year
  
#     working_ndvi_yr = site_data$NDVI  # get ndvi just for ALL year
#     working_gcc_yr = site_data$GCC # get gcc just for ALL year
#     
    # delete leap year day
    if (length(II) == 366){
      working_ndvi_yr = working_ndvi_yr[1:365]
      working_gcc_yr = working_gcc_yr[1:365]
    }
  
    y = working_ndvi_yr
    z = working_gcc_yr
#     data <- list(y = working_ndvi_yr,z = working_gcc_yr,n=length(y),x_ic=1,tau_ic=0.05,
#                  a_ndvi=.59,r_ndvi=1.69,a_gcc=3.16,r_gcc=.316,a_add=1.41,r_add=.71,
#                  beta0=beta0, beta1=beta1, beta2 = beta2, beta3 = beta3)
     data <- list(z = working_gcc_yr,n=length(z),x_ic=1,tau_ic=0.05)
    
    # run JAGS model 
    jags.out=RunJAGS(data,n.iter,n.chains)
    jags.out.matrix <- as.matrix(jags.out)
    jags.out.all.years.array[,,count] <-jags.out.matrix
    
  }
  
  # Make filename and save jags output 
  file_name = paste('Jags.SS.out.site',as.character(site_num), 'RData',sep=".")
  save(jags.out.all.years.array, file = file_name)
  
  # Make plots
  make.SS.plots(jags.out.all.years.array,site_data)

  
}