find.extreme.GCC.NDVI <- function(site.number, first.year, last.year,
                                  use.interannual.means=FALSE){
  # The function find.extreme.GCC.NDVI takes three inputs:
  # site.number is the integer number of the site
  # first.year and last.year are integers (for example, 2000 and 2013)
  # 
  # the returned value is a vector containing:
  #   c(ndvi_max, ndvi_min, gcc_max, gcc_min) for the requested date range.
  
  # MINMUM = JAN FEB
  # MAXIMUM = JUL AUG
  
  # NDVI data:
  ndvi.all = read.csv(sprintf("ndvi_data_site%i.csv",site.number))
  years.ndvi = as.numeric(format(as.Date(ndvi.all$date), "%Y"))
  doy.ndvi = as.numeric(format(as.Date(ndvi.all$date), "%j"))
  
  # GCC data:
  gcc.all = read.csv(sprintf("gcc_data_site%i.csv",site.number))
  years.gcc = as.numeric(format(as.Date(gcc.all$date), "%Y"))
  doy.gcc = as.numeric(format(as.Date(gcc.all$date), "%j"))
  
  # use.interannual.means == TRUE
  # get the max/min for each year:
  
  ndvi_max_yr = numeric(length = length(first.year:last.year))
  ndvi_min_yr = numeric(length = length(first.year:last.year))
  gcc_max_yr = numeric(length = length(first.year:last.year))
  gcc_min_yr = numeric(length = length(first.year:last.year))
  
  # counts loop
  count = 0
  
  for(yr in first.year:last.year) {
    count = count + 1;
    
    ndvi.selected = subset(ndvi.all$ndvi, 
                           (years.ndvi >= yr) & (years.ndvi <= yr))
    doy.ndvi.selected = subset(doy.ndvi, 
                               (years.ndvi >= yr) & (years.ndvi <= yr))
    
    gcc.selected = subset(gcc.all$gcc.mean, 
                          (years.gcc >= yr) & (years.gcc <= yr))
    doy.gcc.selected = subset(doy.gcc, 
                              (years.gcc >= yr) & (years.gcc <= yr))
    
    jan_feb_ndvi = which(doy.ndvi.selected < 60 ) # days 1-59
    jan_feb_gcc = which(doy.gcc.selected < 60 )
    
    jul_aug_ndvi = which(doy.ndvi.selected < 244 & doy.ndvi.selected > 181 ) # days 182-243
    jul_aug_gcc = which(doy.gcc.selected < 244 & doy.gcc.selected > 181)
    
    ndvi_max_i = suppressWarnings(median(ndvi.selected[jul_aug_ndvi],na.rm=TRUE))
    ndvi_min_i = suppressWarnings(median(ndvi.selected[jan_feb_ndvi],na.rm=TRUE))
    gcc_max_i = suppressWarnings(median(gcc.selected[jul_aug_gcc],na.rm=TRUE))
    gcc_min_i = suppressWarnings(median(gcc.selected[jan_feb_gcc],na.rm=TRUE))
    
    ndvi_max_yr[count] = ndvi_max_i
    ndvi_min_yr[count] = ndvi_min_i
    gcc_max_yr[count] =  gcc_max_i
    gcc_min_yr[count] = gcc_min_i
    
  } # end for loop over years
  
  # IGNORE WARNINGS, Angela's got this... =)
  ndvi_max = suppressWarnings(median(ndvi_max_yr,na.rm=TRUE))
  ndvi_min = suppressWarnings(median(ndvi_min_yr,na.rm=TRUE))
  gcc_max = suppressWarnings(median(gcc_max_yr,na.rm=TRUE))
  gcc_min = suppressWarnings(median(gcc_min_yr,na.rm=TRUE))    
  
  if (is.na(gcc_min)){
    gcc_min =  suppressWarnings(min(subset(gcc.all$gcc.90, (years.gcc < last.year)) ,na.rm=TRUE))
  }
  if (is.na(ndvi_min)){
    ndvi_min = suppressWarnings(min(subset(ndvi.all$ndvi, (years.ndvi < last.year)), na.rm=TRUE))
  }
  if (is.na(gcc_max)){
    gcc_max = suppressWarnings(max(subset(gcc.all$gcc.90, (years.gcc < last.year)),na.rm=TRUE))
  }
  if (is.na(ndvi_max)){
    ndvi_max = suppressWarnings(max(subset(ndvi.all$ndvi, (years.ndvi < last.year)),na.rm=TRUE))
  }   
  
  max_min_ndvi_gcc = c(ndvi_max,ndvi_min,gcc_max,gcc_min)
  
  # if(any(is.infinite(max_min_ndvi_gcc))){
  #   # warning("find.extreme.GCC.NDVI is returning NA values because there was no data in the date range given for this site!\n")
  #   # Change inf to NA:
  #   max_min_ndvi_gcc[is.infinite(max_min_ndvi_gcc)] = NA
  # }
  
  return(max_min_ndvi_gcc)
  
}