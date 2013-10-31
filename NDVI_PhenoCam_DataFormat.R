# NDVI

dat = read.table("phen_sites.csv",header = TRUE, sep=",") # dat has site specs
file_name = levels(dat$save_dir)

year = as.numeric(format(Sys.time(), "%Y"))
year_start = year - 3; # Assuming the first year is 2010...
LAT = dat$lat
LON = dat$lon

for (i in 1) {
  
    # load modis raw data 
    modis_filename = paste('Lat',as.character(LAT[i]),'0Lon',as.character(LON[i]),
                       '00Start',year,'-01-01End',year,'-12-31_MOD09A1.asc',sep = "")

    modis_dat = read.csv(modis_filename,header=FALSE)
    
    id_num = unique(modis_dat$V5)
    id_num <- id_num[!is.na(id_num)]
    
    band_1_data= rep(NA,length(id_num))
    band_2_data = rep(NA,length(id_num))
    DOY_data = rep(NA,length(id_num))
    
    for (p in 1:length(id_num) ) {

      day_index = which(id_num[p]==modis_dat$V5) # && column1 ==? day of year)
      day_data = modis_dat[as.numeric(day_index),]
      
      band_1_index = grep('b01',day_data$V1)
      band_2_index = grep('b02',day_data$V1)
      DOY_index = grep('day_of_year',day_data$V1)
      
      band_1_data[p] = mean(as.numeric(day_data[band_1_index,6:ncol(day_data)]))
      band_2_data[p] = mean(as.numeric(day_data[band_2_index,6:ncol(day_data)]))
      DOY_data[p] = day_data[DOY_index,6]
      year[p]  
    
    }
    
    NDVI = (band_2_data - band_1_data) / (band_2_data + band_1_data)
    
    
    }

    # load phenocam data
    pheno_filename = file_name[i]
    pheno_dat = read.csv(pheno_filename,header=TRUE,skip=6) # first 6 lines are not useful

    
}  