last.date.gcc.data <- function(site_num) {
##### get last date of forecast data being used for GCC 
## load GCC data
file_name_gcc <- paste('gcc_data_site',as.character(site_num), 'csv',sep=".")
gcc.data <- read.csv(file_name_gcc)
not.NA.values.gcc <- !is.na(gcc.data[,4]) ## all non-NA mean GCC values
count.gcc <- 1:length(not.NA.values.gcc)
last.gcc.point <- max(count.gcc[not.NA.values.gcc])
gcc.data.dates <- gcc.data[,1]
last.gcc.date = as.Date(gcc.data.dates[last.gcc.point])
return(last.gcc.date)
}