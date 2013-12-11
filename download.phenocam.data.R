download.phenocam.data <- function(site.number) {
  
  # Read in the info on where to get phenocam and MODIS data:
  site.metadata <- read.table("site_metadata.csv",header = TRUE, 
                              sep=",",stringsAsFactors=FALSE) # site name, phenocam url, lat, lon
  
  phenocam.url <- site.metadata$phenocam_url[site.number]
  sys_command = sprintf("wget -O phenocam_data_site%i.csv %s", site.number, phenocam.url)
  system(sys_command)
  
}