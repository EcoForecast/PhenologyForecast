download.all.modis.data <- function(site.number) {
  # Downloads all of the modis data for the site site.number
  
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
  
  # to get all data:
  modis.date.range = GetDates(Product = "MOD09A1", Lat = site.lat, Long = site.lat) # gets date range of modis data
  
  start.date.string = modis.date.range[1] # Extracts first date
  end.date.string = modis.date.range[length(modis.date.range)] # Extracts last date
  
  first.year.with.data = as.numeric(substr(start.date.string,2,5)) # Gets year of first date 
  last.year.with.data = as.numeric(substr(end.date.string,2,5)) # Gets year of last date 
  
  # Download MODIS data
  print(sprintf("Downloading MODIS data for site %i.\n",site.number))
  MODISSubsets(data.frame(lat = site.lat,
                          long = site.lon,
                          start.date = first.year.with.data,
                          end.date = last.year.with.data),
               Product="MOD09A1",Bands=c("sur_refl_day_of_year","sur_refl_qc_500m",
                                         "sur_refl_state_500m","sur_refl_vzen",
                                         "sur_refl_b01","sur_refl_b02"),
               Size=c(1,1), SaveDir = ".", StartDate=TRUE)
  
    
}