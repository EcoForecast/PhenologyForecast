## Get MODIS Data 
# phen_sites = data.frame( SITE_NAME = c("Coweeta","Shalehillsczo","Howland","Shenandoah","Bartlett"),
#                        URL = c("http://phenocam.sr.unh.edu/data/archive/bartlett/ROI/bartlett_deciduous_0001_gcc.csv",
#                                "http://phenocam.sr.unh.edu/data/archive/coweeta/ROI/coweeta_deciduous_0002_gcc.csv",         
#                                "http://phenocam.sr.unh.edu/data/archive/howland1/ROI/howland1_canopy_0001_gcc.csv",          
#                                "http://phenocam.sr.unh.edu/data/archive/shalehillsczo/ROI/shalehillsczo_canopy_0001_gcc.csv",
#                                "http://phenocam.sr.unh.edu/data/archive/shenandoah/ROI/shenandoah_canopy_0001_gcc.csv"),
#                        save_dir = c("bartlett_deciduous_0001_gcc.csv",
#                                     "coweeta_deciduous_0002_gcc.csv",         
#                                     "howland1_canopy_0001_gcc.csv",          
#                                     "shalehillsczo_canopy_0001_gcc.csv",
#                                     "shenandoah_canopy_0001_gcc.csv"),
#                        lat = c(35.0596, 40.6500, 45.2041, 38.5926, 44.0646), 
#                        lon = c(-83.4280, -77.9000, -68.7403, -78.3756, -71.2881))
#
# write.table(dat,"phen_sites.csv",row.names=FALSE,sep=",") 


loaded <- require('devtools')
if(!loaded){
  print("trying to install devtools")
  install.packages("devtools")
  loaded <- require('devtools')
  if(loaded){
    print("devtools installed and loaded")
  } else {
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
  } else {
    stop("could not install MODISTools")
  }    
}


dat = read.table("phen_sites.csv",header = TRUE, sep=",") # dat has site specs

LAT = dat$lat
LON = dat$lon
URL_str = levels(dat$URL)
file_name = levels(dat$save_dir)

######## ######## ######## ######## SET YOUR OWN PATH ######## ######## ######## ######## 
#save_path = "~/Documents/R/PhenologyForecast/PhenologyForecast" #to test on Angela's computer
save_path = "/var/www/ge585/"
######## ######## ######## ######## ######## ######## ######## ######## ######## ######## 

# Initializing arrays to save the start and end year of modis data
year_start = rep(NA,5)
year_end = rep(NA,5)

for (i in 1:5 ) {
  
  # to get all data:
  YR_DOY = GetDates(Product = "MOD09A1", Lat = LAT[i], Long = LON[i]) # gets date range of modis data
  
  start_yr_doy_str = YR_DOY[1] # Extracts first date
  end_yr_doy_str = YR_DOY[length(YR_DOY)] # Extracts last date
  
  year_start[i] = as.numeric(substr(start_yr_doy_str,2,5)) # Gets year of first date 
  year_end[i] = as.numeric(substr(end_yr_doy_str,2,5)) # Gets year of last date 
  
  # Download Modis data
  MODISSubsets(data.frame(lat=LAT[i],long=LON[i],start.date=year_start[i],end.date=year_end[i]),
               Product="MOD09A1",Bands=c("sur_refl_day_of_year","sur_refl_qc_500m",
                                         "sur_refl_state_500m","sur_refl_vzen",
                                         "sur_refl_b01","sur_refl_b02"),
               Size=c(1,1), SaveDir = save_path, StartDate=TRUE)
  
  # Downloads phenocam data
  sys_command = paste("wget", URL_str[i])
  system(sys_command)

  # moves phenocam data and saves it to save_path
  sys_command_save = paste("mv",file_name[i],save_path)
  system(sys_command_save)
  
}

# Makes a table of year range to be used in data formatting, 5 x 3, first col = site name, second column = start year, third column = end year
modis_dat_range = data.frame(SITE_NAME = c("Coweeta","Shalehillsczo","Howland","Shenandoah","Bartlett"),
                         modis_start_year = year_start, modis_end_year = year_end)

write.table(modis_dat_range,"modis_dat_range.csv",row.names=FALSE,sep=",") 



