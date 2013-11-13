# NDVI

# loads the site details
dat = read.table("phen_sites.csv",header = TRUE, sep=",") # dat has site specs
file_name = levels(dat$save_dir)

# loads the data range for modis data 
modis_dat_range = read.table("modis_dat_range.csv",header = TRUE, sep=",") 

# Extracts lat, lon, start year, end year as vectors (just cause I like vectors)
LAT = dat$lat
LON = dat$lon
ST_YR = modis_dat_range$modis_start_year  
ED_YR = modis_dat_range$modis_end_year  

# Intitializes data frame for every day of the year at every site. 
HISTORICAL_DATA <- data.frame()

for (i in 1:5) { # for loop over sites
  
    year_end = ED_YR[i] 
    year_start =  ST_YR[i] 
    lat_str = sprintf('%1.5f',LAT[i]) # The lat and lon need to have 5 decimal places (WITH trailing zeros included)
    lon_str = sprintf('%1.5f',LON[i])
    
    # make filename of modis data
    modis_filename = paste('Lat',lat_str,'Lon',lon_str,
                       'Start',year_start,'-01-01End',year_end,'-12-31_MOD09A1.asc',sep = "")
    
    # load modis data as csv
    modis_dat = read.csv(modis_filename,header=FALSE)
    
    # find the unique ID number. There is a unique ID number associated with everyday (shared by all vars for that day)
    id_num = unique(modis_dat$V5)
    id_num <- id_num[!is.na(id_num)]
    year_dat = as.numeric(substr(id_num,1,4)) # find the year associated with that day
    
    # Initialize arrays
    band_1_data= rep(NA,length(id_num))
    band_2_data = rep(NA,length(id_num))
    DOY_data = rep(NA,length(id_num))
    date_format = as.Date(rep(NA,length(id_num)))
    site_ID = rep(i,length(id_num)) # Just for ease, assign ID number to each site (1:5, rows of phen_sites.csv)
    
    for (p in 1:length(id_num) ) { # for loop over each day.  

      day_index = which(id_num[p]==modis_dat$V5) # for each ID number, find the rows that match that ID number (i.e. from the same day)
      day_data = modis_dat[as.numeric(day_index),] # extract the modis data for that index (that day)
      
      band_1_index = grep('b01',day_data$V1) # get index band 1
      band_2_index = grep('b02',day_data$V1) # get index band 2
      DOY_index = grep('day_of_year',day_data$V1) # get index DOY
      
      band_1_data[p] = mean(as.numeric(day_data[band_1_index,6:ncol(day_data)])) # Average over all pixels
      band_2_data[p] = mean(as.numeric(day_data[band_2_index,6:ncol(day_data)])) # Average over all pixels
      DOY_data[p] = day_data[DOY_index,6] 
      
      date_info = paste(year_dat[p],DOY_data[p]) # Convert DOY, year to date
      date_format[p] = strptime(date_info, "%Y %j")
    }
    
    NDVI_cal = (band_2_data - band_1_data) / (band_2_data + band_1_data) # Calculate NDVI
    
    MODIS_DATA_ST <- data.frame(site_id = site_ID, date = date_format, NDVI = NDVI_cal)
    # Need to delete leap days...
    leap_days <- is.na(MODIS_DATA_ST)
    MODIS_DATA_ST <- as.data.frame(subset.data.frame(MODIS_DATA_ST,!leap_days[,2]))
    # leap days deleted!      
        
    # load phenocam data
    pheno_filename = file_name[i]
    pheno_dat = read.csv(pheno_filename,header=TRUE,skip=6) # first 6 lines are not useful
    
    # Vector of possible historical dates (satellit was launched in 2000...)
    days_possible = seq(as.Date("2000-01-01"), as.Date("2013-12-31"), by="days")
    
    # Finds indices of dates of modis and phenocam data that are observed (and match possible_days)
    PHEN_IND = match(as.Date(pheno_dat$date),days_possible)
    MOD_IND = match(MODIS_DATA_ST$date,days_possible)
    
    # make times series vector of phenocam and modis data 
    phen_date = rep(NA,length(days_possible))
    phen_date[PHEN_IND] = pheno_dat$gcc_mean
    
    mod_date = rep(NA,length(days_possible))
    mod_date[MOD_IND] = MODIS_DATA_ST$NDVI  
    
    # makes of vector of site ID
    site_id = rep(i,length(days_possible))
    
    # Makes data fram of individual site data 
    MODIS_PHEN_DATA_ST = data.frame(site_ID = site_id, date = days_possible, NDVI = mod_date, GCC = phen_date)
    
    # Concatenates all data into one frame... site is specificed by site ID
    HISTORICAL_DATA = rbind.data.frame(HISTORICAL_DATA,MODIS_PHEN_DATA_ST) # Just to see all MODIS data
    
    }

write.csv(HISTORICAL_DATA,file="full_historical_data.csv")  




    
  