last.date.ndvi.data <- function(site_num) {
##### get last date of forecast data being used for NDVI
## load NDVI data
file_name_ndvi <- paste('ndvi_data_site',as.character(site_num), 'csv',sep=".")
ndvi.data <- read.csv(file_name_ndvi)
not.NA.values.ndvi <- !is.na(ndvi.data[,4]) ## all non-NA mean GCC values
count.ndvi <- 1:length(not.NA.values.ndvi)
last.ndvi.point <- max(count.ndvi[not.NA.values.ndvi])
ndvi.data.dates <- ndvi.data[,1]
last.ndvi.date <- as.Date(ndvi.data.dates[last.ndvi.point])
return(last.ndvi.date)
} 