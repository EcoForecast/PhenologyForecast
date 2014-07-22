download.all.modis.data <- function(site.number) {
  # Downloads all of the modis data for the site site.number
  
  source("global_input_parameters.R") # For burn-in
  useMODISTools = global_input_parameters$useMODISTools
  
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
  
  havePEcAn = require('PEcAn.data.remote')
  
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
  
  start.date = as.integer(substr(start.date.string,2,8))
  end.date = as.integer(substr(end.date.string,2,8))
  
  # Download MODIS data
  print(sprintf("Downloading MODIS data for site %i.\n",site.number))
  if(useMODISTools){
    MODISSubsets(data.frame(lat = site.lat,
                          long = site.lon,
                          start.date = first.year.with.data,
                          end.date = last.year.with.data),
               Product="MOD09A1",Bands=c("sur_refl_day_of_year","sur_refl_qc_500m",
                                         "sur_refl_state_500m","sur_refl_vzen",
                                         "sur_refl_b01","sur_refl_b02"),
               Size=c(1,1), SaveDir = ".", StartDate=TRUE)
  }else{
    if(havePEcAn){
      doy = call_MODIS("./MODIS",paste0(site.number,".DOY.nc"),start = start.date,end = end.date,
                 lat = site.lat,lon=site.lon,product="MOD09A1",band="sur_refl_day_of_year")

      qc = call_MODIS("./MODIS",paste0(site.number,".qc.nc"),start = start.date,end = end.date,
                         lat = site.lat,lon=site.lon,product="MOD09A1",band="sur_refl_qc_500m")
      
      state = call_MODIS("./MODIS",paste0(site.number,".state.nc"),start = start.date,end = end.date,
                       lat = site.lat,lon=site.lon,product="MOD09A1",band="sur_refl_state_500m")

      vzen = call_MODIS("./MODIS",paste0(site.number,".vzen.nc"),start = start.date,end = end.date,
                         lat = site.lat,lon=site.lon,product="MOD09A1",band="sur_refl_vzen")
      
      b01 = call_MODIS("./MODIS",paste0(site.number,".b01.nc"),start = start.date,end = end.date,
                        lat = site.lat,lon=site.lon,product="MOD09A1",band="sur_refl_b01")

      b02 = call_MODIS("./MODIS",paste0(site.number,".b02.nc"),start = start.date,end = end.date,
                       lat = site.lat,lon=site.lon,product="MOD09A1",band="sur_refl_b02")
      
      site.data = list(date=doy$date,qc=unlist(qc$m),doy = unlist(doy$m),state = unlist(state$m),vzen=unlist(vzen$m),b01=unlist(b01$m),b02=unlist(b02$m))
      
      save(site.data,file=paste0("./MODIS/",site.number,".RData"))
      
    }
  }
  
    
}