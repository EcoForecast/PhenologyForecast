# A high-level outline of forecasting structure
setwd("/home/dietze/PhenologyForecast/")

# All site information is read from the file "site_metadata.csv". 
# Analysis will be performed on each site in that file.
num.sites = as.numeric(nrow(read.csv("site_metadata.csv")))

# For each site, we need to run a state-space model, and then the forecast model for the current year
for(site in 1:num.sites) {
  
  # Step 1: Download/update site GCC and MODIS data
  source("get.site.data.R")
  get.site.data(site)
  
  # Step 2: check to see if state space model has already been run
  source("check.for.complete.SS.model.R")
  SS.complete <- check.for.complete.SS.model(site) # SS.complete is TRUE/FALSE 
  
  # If not, then create the state space model for that site:
  if(!SS.complete) {
    source("run.SS.model.R")
    run.SS.model(site) # This should create some files which contain inputs for the forecast model
  }
  
  # Step 3: Check to see if the forecast model has been run (and has some output)
  # "FM" means forecast model, the particle filter in this case
  source("check.for.FM.model.R")
  FM.complete <- check.for.FM.model(site) # FM.created is TRUE/FALSE 
  
  # If not, then create the forecast model for that site:
  if(!FM.complete) {
    source("create.FM.model.R")
    create.FM.model(site) # This should create some files with forecasts and some other output
  }
  
  # Step 4: Check for new data not yet included in forecast model:
  source("update.FM.model.R")
  update.FM.model(site) # This should update the output from the forecast model for any new data
  
}

# Also, plot all of the data:
source("PlotDataTimeSeries.R")

## open most recent forecast
source("global_input_parameters.R")
model.start.DOY <- global_input_parameters$model.start.DOY
model = global_input_parameters$model
last.date.filename <- paste("last.update.site", as.character(site.number), model,"txt",sep=".")
read.in <- source(last.date.filename)
last.forecast.date <- as.Date(read.in$value)
output_file_name = paste0("forecastRData/",paste("ForecastModel.X.out.site", as.character(site.number),model,last.forecast.date, 
                                                 "RData",sep="."))
load(output_file_name)
p[p<=0] = NA
Tday = which.min((182:365-p[,3])^2)+181

## plot threshold forecast
plot(182:365,182:365,type='n',xlab="DOY",ylab="Threshold Day")
abline(0,1)
lty=c(3,2,1,2,3)
for(i in 1:5){
  lines(182:365,p[,i],lty=lty[i])
}
abline(v=Tday,col=2)

#IQR
plot(182:365,p[,4]-p[,2],xlab="DOY",ylab="IQR (days)")
abline(v=Tday,col=2)

X.ci  = apply(X,1,quantile,c(0.025,0.25,0.5,0.75,0.975))
plot(model.start.DOY:365,X.ci[3,],type='n',
     main=paste("Particle Filter Forecast:",last.forecast.date+1),
     xlab="Day of Year",ylab="Pheno-state",ylim=c(0,1))
ciEnvelope(model.start.DOY:365,X.ci[1,],X.ci[5,],col="light grey")
ciEnvelope(model.start.DOY:365,X.ci[2,],X.ci[4,],col="grey")
abline(v=Tday,col=2)
abline(h=0.5,col=2)
