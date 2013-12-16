download.new.modis.data <- function(site.number) {
  # The function download.new.modis.data looks for the last modis data for the site 
  # site.number, and then downloads any data that comes after that date.
  
  ndvi_filename <- sprintf("ndvi_data_site%i.csv",site.number)
  
  old.ndvi.data <- read.csv(ndvi_filename)
  
  # Find the last non-NA data point in the file
  not.nas <- !is.na(old.ndvi.data$ndvi)
  counter <- 1:length(not.nas)
  last.data.index <- max(counter[not.nas])
  last.data.date <- as.Date(old.ndvi.data$date[last.data.index])
  
  # If the last data point isn't from today...
  if(last.data.date < Sys.Date()) { 
    # Then we download the modis data:
    
    # Load some required packages:
    loaded <- require('devtools')
    if(!loaded){
      print("trying to install devtools")
      install.packages("devtools")
      loaded <- require('devtools')
      if(loaded){
        print("devtools installed and loaded")
      } 
      else {
        stop("could not install devtools")
      }    
    }
    
    loaded <- require('MODISTools')
    if(!loaded){
      print("trying to install MODISTools")
      install.packages("MODISTools")
      loaded <- require('MODISTools')
      if(loaded){
        print("MODISTools installed and loaded")
      } 
      else {
        stop("could not install MODISTools")
      }    
    }
    
    
    # Read in the info on where to get phenocam and MODIS data:
    site.metadata <- read.table("site_metadata.csv",header = TRUE, 
                                sep=",",stringsAsFactors=FALSE) # site name, phenocam url, lat, lon
    
    site.lat <- site.metadata$lat[site.number]
    site.lon <- site.metadata$lon[site.number]
    
    first.year <- as.numeric(format(last.data.date, "%Y"))
    last.year <-  as.numeric(format(Sys.Date(), "%Y"))
    
    # Download MODIS data
    print(sprintf("Downloading MODIS data for site %i.",site.number))
    MODISSubsets(data.frame(lat = site.lat,
                            long = site.lon,
                            start.date = first.year,
                            end.date = last.year),
                 Product="MOD09A1",Bands=c("sur_refl_day_of_year","sur_refl_qc_500m",
                                           "sur_refl_state_500m","sur_refl_vzen",
                                           "sur_refl_b01","sur_refl_b02"),
                 Size=c(1,1), SaveDir = ".", StartDate=TRUE)
    
  }
  else{ # No new data to download...
    return()
  }
  
}

