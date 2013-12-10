get.site.data <- function(site.number) {
  # The function get.site.data takes an integer input corresponding to the
  # sites listed in the file phen_sites.csv and creates/updates the GCC and
  # MODIS data files for that site. 
  
  gcc_filename <- sprintf("gcc_data_site%i,csv",site.number)
  ndvi_filename <- sprintf("gcc_data_site%i,csv",site.number)
  
  # Check to see if some data has already been downloaded (i.e. if the files
  # already exist):
  some.data.downloaded <- file.exists(gcc_filename) &&  file.exists(ndvi_filename) # some.data.downloaded is TRUE/FALSE
  
  # If none of the data for that site has been downloaded, then download all
  # of the gcc and MODIS data from 2000-present, and process it. Save the 
  # data in the file SOMETHING2.CSV:
  if(!some.data.downloaded){
    # Read in the info on where to get phenocam and MODIS data:
    site.metadata <- read.table("site_metadata.csv",header = TRUE, 
                                sep=",",stringsAsFactors=FALSE) # site name, phenocam url, lat, lon
        
    # Download phenocam data:
    phenocam.url <- site.metadata$phenocam_url[site.number]
    sys_command = sprintf("wget -O phenocam_data_site%i.csv %s", site.number, phenocam.url)
    system(sys_command)
    
    # Process phenocam data:
    
    # Save gcc data:
    
    # Download MODIS data:
    
    # Process MODIS data:
    
    # Save NDVI data:
    
  }
  else{ #ie if some.data.downloaded is TRUE, just need to update:
    SOME CODE
  }
  
}