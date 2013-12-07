## defines confidence interval function
find.extreme.GCC.NDVI <- function(site_data){
  
  ## pull out NDVI
  ndvi=site_data$NDVI
  
  ## pull out GCC
  gcc=site_data$GCC
  
  # pull out time
  time=site_data$date
  
  # Calculate Min/Max NDVI and GCC to be used in the data model.
  time_year = as.numeric(format(as.Date(time), "%Y"))
  
  # Initialize vector that stores max and min ndvi/gcc in each year (size, 1xnumyr)
  ndvi_max_yr = numeric()
  ndvi_min_yr = numeric()
  gcc_max_yr = numeric()
  gcc_min_yr = numeric()
  
  # counts loop
  count = 0;
  
  for (YR in 2000:2012) {
    
    count = count + 1;
    # gets index of year
    II = which(time_year== YR)
    
    # gets ndvi/gcc max/min... throws warnings because NA for entire year 
    # warning: no non-missing arguments to max; returning -Inf
    # IGNORE WARNINGS, Angela's got this...
    ndvi_max_yr[count] = suppressWarnings(max(ndvi[II],na.rm = TRUE) )
    ndvi_min_yr[count] = suppressWarnings(min(ndvi[II],na.rm = TRUE))
    gcc_max_yr[count] = suppressWarnings(max(gcc[II],na.rm = TRUE))
    gcc_min_yr[count] = suppressWarnings(min(gcc[II],na.rm = TRUE))
    
    
  }

  # find mean of ndvi/gcc max/min... 
  # the "is.finite" is because: max(c(NA, NA),na.rm = TRUE) = +-Inf 
  ndvi_max = mean(ndvi_max_yr[is.finite(ndvi_max_yr)])
  ndvi_min = mean(ndvi_min_yr[is.finite(ndvi_min_yr)])
  gcc_max = mean(gcc_max_yr[is.finite(gcc_max_yr)])
  gcc_min = mean(gcc_min_yr[is.finite(gcc_min_yr)])
  
  max_min_ndvi_gcc = c(ndvi_max,ndvi_min,gcc_max,gcc_min)
  return(max_min_ndvi_gcc)
  
    
    
  }