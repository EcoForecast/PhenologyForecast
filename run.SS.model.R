run.SS.model <- function(site_num){
  
  # source functions
  source("find.extreme.GCC.NDVI.R")
  source("RunJAGS.R")
  source("make.SS.plots.R")
  
  # load site gcc and ndvi data:
  gcc.filename <- sprintf("gcc_data_site%i.csv",site_num)
  ndvi.filename <- sprintf("ndvi_data_site%i.csv",site_num)
  gcc.data <- read.csv(gcc.filename)
  ndvi.data <- read.csv(ndvi.filename)
  
  # combine them, to make sure that dates match properly (they should already, but
  # just in case...)
  all.data <- merge(gcc.data,ndvi.data)
  
  # Define fall dates:
  source("global_input_parameters.R")
  first.day.of.season <- global_input_parameters$model.start.DOY 
  day.of.year <- as.numeric( strftime(all.data$date, format="%j") ) # a vector of the DOY for each date
  
  # We are only going to use the fall data:
  fall.days <- (day.of.year >= first.day.of.season) & (day.of.year < 366) # solves the leap year problem
  fall.data = all.data[fall.days,]
  
  ## build data object for JAGS in year loop. 
  # pull out time, get year for each data point. 
  time=fall.data$date
  time_year = as.numeric(format(as.Date(time), "%Y"))
  
  
  # Define the number of iterations and chains
  n.iter = global_input_parameters$number.of.SS.model.iterations;
  n.chains = global_input_parameters$number.of.SS.model.chains;
  
  # Stores output from each year
  num.years.data <- length(unique(time_year)) # number of years (including the current
  # year which won't go into the SS model)
  
  # output parameters: [r  tau_add	tau_gcc	tau_ndvi	x], x will use (366-first.day.of.season columns)
  jags.out.all.years.array = array(rep(NA, n.iter*n.chains*(4+366-first.day.of.season)*(num.years.data-1)),
                                   c(n.iter*n.chains,(4+366-first.day.of.season),(num.years.data-1))) 
  
  
  # find max/min of ndvi and gcc over all years of record except current
  # outputs (ndvi_max,ndvi_min,gcc_max,gcc_min)
  max_min_ndvi_gcc = find.extreme.GCC.NDVI(site_num, min(time_year), max(time_year)-1, 
                                           use.interannual.means=TRUE)
  ndvi_max = max_min_ndvi_gcc[1]
  ndvi_min = max_min_ndvi_gcc[2]
  gcc_max = max_min_ndvi_gcc[3]
  gcc_min = max_min_ndvi_gcc[4]
  # Rescale data to be between 0 and 1 (using max and min NDVI, GCC values from 
  # all years except current year):
  rescaled_NDVI <- (fall.data$ndvi-ndvi_min)/(ndvi_max-ndvi_min)
  rescaled_GCC <- (fall.data$gcc.90-gcc_min)/(gcc_max-gcc_min) # using gcc90
  
  # counts for loop                                     
  count = 0
  for (YR in min(time_year):(max(time_year)-1)) { # loop over all years except current year
    
    cat(sprintf("Running state-space model for site %i, year %i\n\n",site_num,YR))
    count = count + 1;
    # gets index of year
    II = which(time_year == YR)
    rescaled_NDVI_one_year = rescaled_NDVI[II]  # get ndvi just for ONE year
    rescaled_GCC_one_year = rescaled_GCC[II]    # get gcc just for ONE year
    
    # Make list of "data" to be used as input for RunJAGS
    x_ic <- global_input_parameters$x_ic
    tau_ic <- global_input_parameters$tau_ic
    a_ndvi <- global_input_parameters$a_ndvi
    r_ndvi <- global_input_parameters$r_ndvi
    a_gcc <- global_input_parameters$a_gcc
    r_gcc <- global_input_parameters$r_gcc
    a_add <- global_input_parameters$a_add
    r_add <- global_input_parameters$r_add
    
    data <- list(y = rescaled_NDVI_one_year,z = rescaled_GCC_one_year,
                 n=length(rescaled_NDVI_one_year),x_ic=x_ic, tau_ic=tau_ic,
                 a_ndvi=a_ndvi, r_ndvi=r_ndvi, a_gcc=a_gcc, r_gcc=r_gcc,
                 a_add=a_add, r_add=r_add)
    
    # run JAGS model 
    jags.out=RunJAGS(data,n.iter,n.chains)
    jags.out.matrix <- as.matrix(jags.out)
    jags.out.all.years.array[,,count] <- jags.out.matrix
    
  }
  
  # Make filename and save jags output 
  file_name = paste('Jags.SS.out.site',as.character(site_num), 'RData',sep=".")
  save(jags.out.all.years.array, file = file_name)
  
  # Make plots
  make.SS.plots(jags.out.all.years.array,fall.data$date,rescaled_NDVI,rescaled_GCC,site_num)
  
  
}