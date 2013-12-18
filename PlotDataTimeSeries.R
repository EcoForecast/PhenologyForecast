# Plot our input data over time:

pdf("DataTimeSeries.pdf")

for(site.number in 1:5) {
  
  # Load the NDVI data
  filename <- paste("ndvi_data_site",site.number,".csv",sep="")
  ndvi.data <- read.csv(filename)
  
  # Load the GCC data
  filename <- paste("gcc_data_site",site.number,".csv",sep="")
  gcc.data <- read.csv(filename)

  # For our sites, there's no data before 2005, so let's only plot from 2005 on:
  start.idx <- as.numeric(which(ndvi.data$date == "2005-01-01"))
  
  # Plot the data
  plot(as.Date(ndvi.data$date), ndvi.data$ndvi, type='n', 
       main=paste("NDVI and GCC data (unscaled) for site", site.number),
       xlim=as.Date(c("2005-01-01","2013-12-31")))
  points(as.Date(ndvi.data$date), ndvi.data$ndvi, pch="+", cex=0.8, col="dark red")
  points(as.Date(gcc.data$date), gcc.data$gcc.90, pch="o", cex=0.5, col="dark green")
  legend("topleft",c("NDVI","GCC"),pch=c("+","o"), col=c("dark red","dark green")) 
}


dev.off()