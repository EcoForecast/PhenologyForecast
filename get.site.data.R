get.site.data <- function(site.number) {
  # The function get.site.data takes an integer input corresponding to the
  # sites listed in the file site_metadata.csv and creates/updates the GCC and
  # MODIS data files for that site. 
  
  gcc_filename <- sprintf("gcc_data_site%i.csv",site.number)
  ndvi_filename <- sprintf("gcc_data_site%i.csv",site.number)
  
  # Check to see if some data has already been downloaded (i.e. if the files
  # already exist):
  some.data.downloaded <- file.exists(gcc_filename) &&  file.exists(ndvi_filename) # some.data.downloaded is TRUE/FALSE
  
  # If there isn't at least SOME GCC and MODIS data, then download all
  # of the gcc and MODIS data from 2000-present, and process it. Save the 
  # data in the file SOMETHING2.CSV:
  if(!some.data.downloaded){
        
    # Download phenocam data and save (creates file named "phenocam_data_siteX.csv", 
    # where X is the site number):
    download.phenocam.data(site.number)
        
    # Process phenocam data into gcc data:
    create.gcc.data(site.number) # creates file gcc_data_siteX.csv, where X = site.number
        
    # Before we download the MODIS data, we'll remove the file that we'll 
    # get a filename from:
    unlink("Subset Download*.csv")
    
    # Download ALL MODIS data:
    download.all.modis.data(site.number)
    
    # Process MODIS data into NDVI data:
    create.ndvi.data(site.number)    
  }
  else{ #ie if some.data.downloaded is TRUE, just need to update:

    # Probably simplest to just re-download all of the gcc data:
    download.phenocam.data(site.number)
    create.gcc.data(site.number) # creates file gcc_data_siteX.csv, where X = site.number
    
    
    # Just need to download the last year of MODIS data (SUPER SLOW!!), and then add
    # it with the existing data:
    
    
  }
  
}