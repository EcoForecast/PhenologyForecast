## Get MODIS Data 

pkg = rownames(installed.packages())
if(!("devtools" %in% pkg)){
  install.packages("devtools",dependencies=TRUE)
}
library("devtools")

if(!("MODISTools" %in% pkg)){
  install_github("MODISTools","seantuck12")  ## arguements are the library name and the username of the developer
}
library("MODISTools")

GetProducts()
GetBands(Product="MOD13Q1")

# INDEX 
# 1-Coweeta
# 2-Shalehillsczo
# 3-Howland
# 4-Shenandoah
# 5-Bartlett

LAT = c(35.0596, 40.6500, 45.2041, 38.5926, 44.0646)
LON = c(-83.4280, -77.9000, -68.7403, -78.3756, -71.2881)

for (i in 1:5) {
  MODISSubsets(data.frame(lat=LAT[i],long=LON[i],start.date=2010,end.date=2013),
               Product="MOD13Q1",Bands=c("250m_16_days_EVI","250m_16_days_VI_Quality"),Size=c(1,1),StartDate=TRUE)
}


