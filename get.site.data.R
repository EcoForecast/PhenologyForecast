get.site.data <- function(site.number) {
  # The function get.site.data takes an integer input corresponding to the
  # site numbers in the file SOMETHING1.CSV and creates/updates the GCC and
  # MODIS data file stored in SOMETHING2.CSV 
  
  # Check to see if some data has already been downloaded (i.e. if the file
  # SOMETHING2.CSV exists):
  some.data.downloaded <- SOME CODE # some.data.downloaded is TRUE/FALSE
  
  # If none of the data for that site has been downloaded, then download all
  # of the gcc and MODIS data from 2000-present, and process it. Save the 
  # data in the file SOMETHING2.CSV:
  if(!some.data.downloaded){
    SOME CODE
  }
  else{ #ie if some.data.downloaded is TRUE, just need to update:
    SOME CODE
  }
  
}