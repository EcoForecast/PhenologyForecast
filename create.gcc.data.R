create.gcc.data <- function(site.number){
  # takes the data in the file phenocam_data_siteX.csv (where X is the site number)
  # and processes it into GCC data, saved in gcc_data_siteX.csv.
  
  # Load the phenocam data:
  phenocam_data_file <- sprintf("phenocam_data_site%i.csv",site.number)
  pheno_dat = read.csv(phenocam_data_file,header=TRUE,skip=6) # first 6 lines are not useful
  
  # Create a vector of possible data observation dates
  source("global_input_parameters.R")
  start.date <- as.Date(global_input_parameters$data.start.date)
  current.year <- format(Sys.time(), "%Y")
  end.date <- as.Date(paste(current.year,"-12-31",sep=""))
  daily.dates = seq(start.date, end.date, by="days")
  
  # Finds indices of dates of phenocam data that are observed (and match possible_days)
  days.with.gcc.data = match(as.Date(pheno_dat$date),daily.dates)
  
  # make times series vector of phenocam data, the same length as daily.dates
  gcc.min = rep(NA,length(daily.dates))
  gcc.max = rep(NA,length(daily.dates))
  gcc.mean = rep(NA,length(daily.dates))
  gcc.90 = rep(NA,length(daily.dates)) # This is the 90th percentile (ie, on the 
                                      # higher end) of the observed gcc data for 
                                      # that day
  gcc.min[days.with.gcc.data] = pheno_dat$gcc_min
  gcc.max[days.with.gcc.data] = pheno_dat$gcc_max
  gcc.mean[days.with.gcc.data] = pheno_dat$gcc_mean
  gcc.90[days.with.gcc.data] = pheno_dat$gcc_90
  
  # Makes data frame of GCC time series with dates 
  GCC.data = data.frame(date = daily.dates, gcc.min = gcc.min, gcc.max = gcc.max, 
                        gcc.mean = gcc.mean, gcc.90 = gcc.90)
  
  # Save GCC data:
  write.csv(GCC.data, file = sprintf("gcc_data_site%i.csv",site.number),row.names=FALSE)
  
}