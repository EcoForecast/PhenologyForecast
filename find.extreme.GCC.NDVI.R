find.extreme.GCC.NDVI <- function(site.number, first.year, last.year,
                                  use.interannual.means=FALSE){
  # The function find.extreme.GCC.NDVI takes three inputs:
  # site.number is the integer number of the site
  # first.year and last.year are integers (for example, 2000 and 2013)
  # 
  # the returned value is a vector containing:
  #   c(ndvi_max, ndvi_min, gcc_max, gcc_min) for the requested date range.
  #
  # If you pass the optional input use.interannual.means=TRUE, the extreme values 
  # returned will not be the true min/max for that data range, but will instead be
  # the mean values taken across the min/max values from each year.
  
  if(use.interannual.means == FALSE){
    # NDVI data:
    ndvi.all = read.csv(sprintf("ndvi_data_site%i.csv",site.number))
    years.ndvi = as.numeric(format(as.Date(ndvi.all$date), "%Y"))
    ndvi.selected = subset(ndvi.all$ndvi, 
                           (years.ndvi >= first.year) & (years.ndvi <= last.year))
    
    # GCC data:
    gcc.all = read.csv(sprintf("gcc_data_site%i.csv",site.number))
    years.gcc = as.numeric(format(as.Date(gcc.all$date), "%Y"))
    gcc.selected = subset(gcc.all$gcc.mean, 
                          (years.gcc >= first.year) & (years.gcc <= last.year))
    
    
    
    # gets ndvi/gcc max/min... throws warnings because NA for entire year 
    # warning: no non-missing arguments to max; returning -Inf
    # IGNORE WARNINGS, Angela's got this...
    ndvi_max = suppressWarnings(max(ndvi.selected,na.rm=TRUE))
    ndvi_min = suppressWarnings(min(ndvi.selected,na.rm=TRUE))
    gcc_max = suppressWarnings(max(gcc.selected,na.rm=TRUE))
    gcc_min = suppressWarnings(min(gcc.selected,na.rm=TRUE))
    
  }
  else { # use.interannual.means == TRUE
    # get the max/min for each year:
    ndvi_max_yr = numeric(length = length(first.year:last.year))
    ndvi_min_yr = numeric(length = length(first.year:last.year))
    gcc_max_yr = numeric(length = length(first.year:last.year))
    gcc_min_yr = numeric(length = length(first.year:last.year))
    
    # counts loop
    count = 0;
    for(yr in first.year:last.year) {
      count = count + 1;
      out.vec = find.extreme.GCC.NDVI(site.number,yr,yr,use.interannual.means=FALSE) # recursion!
      ndvi_max_yr[count] = out.vec[1]
      ndvi_min_yr[count] = out.vec[2]
      gcc_max_yr[count] = out.vec[3]
      gcc_min_yr[count] = out.vec[4]      
    } # end for loop over years
    
    # IGNORE WARNINGS, Angela's got this...    
    ndvi_max = suppressWarnings(mean(ndvi_max_yr,na.rm=TRUE))
    ndvi_min = suppressWarnings(mean(ndvi_min_yr,na.rm=TRUE))
    gcc_max = suppressWarnings(mean(gcc_max_yr,na.rm=TRUE))
    gcc_min = suppressWarnings(mean(gcc_min_yr,na.rm=TRUE))    
    
  }
  
  max_min_ndvi_gcc = c(ndvi_max,ndvi_min,gcc_max,gcc_min)
  if(any(is.infinite(max_min_ndvi_gcc))){
    # warning("find.extreme.GCC.NDVI is returning NA values because there was no data in the date range given for this site!\n")
    # Change inf to NA:
    max_min_ndvi_gcc[is.infinite(max_min_ndvi_gcc)] = NA
  }
 
  return(max_min_ndvi_gcc)
  
}