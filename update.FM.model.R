##### Check for new data not yet included in forecast model
update.FM.model <- function(site_num,nt) {
  
  ##### get last date of forecast data being used for GCC 
  source("last.date.gcc.data")
  last.gcc.date <- last.date.gcc.data(site_num)
  
  ##### get last date of forecast data being used for NDVI
  source("last.date.ndvi.data.R")
  last.ndvi.date <- last.date.ndvi.data(site_num)
  
  ##### if these dates are prior to the current date, there *might* be new data (and might not be!)
        ## so we need to allow for the possibility that there isn't new data, and if there isn't 
        ## new data we don't need to generate a new forecast
  ##### check to see if last dates are before current date and if so update data
  current.date <- Sys.Date()
  
  ##### counter to see if new forecast should be generated (goes above 0 if there is new data)
  new_forecast = 0
  
  ##### if last gcc date is prior to current date, re-download gcc data (easier than isolating newest data)
  if (last.gcc.date < current.date) { 
    print(sprintf("Getting phenocam data for site %i...",site.number))
    source("download.phenocam.data.R")
    download.phenocam.data(site.num)
    print(sprintf("Processing phenocam data for site %i...",site.number))
    source("create.gcc.data.R")
    create.gcc.data(site.num) # creates file gcc_data_siteX.csv, where X = site.number
    
    ## get the last date
    new.last.gcc.date <- last.date.gcc.data(site_num)
    if (new.last.gcc.date > last.gcc.date) {
      new_forecast = new_forecast + 1
    }
  }
  
  ##### if last ndvi date is prior to current date, download just the last year of MODIS data
  ## and then add it with the existing data
  if (last.ndvi.date < current.date) {
  unlink("Subset Download*.csv")
  source("download.new.modis.data.R")
  print(sprintf("Downloading MODIS data for site %i...",site.number))
  download.new.modis.data(site.num)
  print(sprintf("Processing MODIS data for site %i...",site.number))
  source("update.ndvi.data.R")
  update.ndvi.data(site.num)
  
  ## get the last date
  new.last.ndvi.date <- last.date.ndvi.data(site_num)
  if (new.last.ndvi.date > last.ndvi.date) {
    new_forecast = new_forecast + 1
  }
  }
  
  ##### check to see if there is new NDVI or GCC data and if so have to generate a new forecast!! 
  if (new_forecast > 0) {
    ### run particle filter including new data
    update.particle.filter.forecast(site_num)
  
  ### append output to pdf files that were created in the forecast model