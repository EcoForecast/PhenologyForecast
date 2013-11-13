###set path
# load data frame with phenocam and modis data
data = read.csv(full_historical_data.csv)

# sites that we are looking at (indexed by number, so 5 sites)
site_names = unique(data$site_ID)

# number of dates included
num_dates = data$date;

model{

  ### Loop over all sites
  for(i in 1:num_sites){
    data ~ data[data$site_ID==i];
    
    ### loop over all dates
    for (j in 1:num_dates) {
    
    #### Data Model: NDVI
    for(q in 1:num_dates){
      data$NDVI[q] ~ dnorm(data$NDVI[q],tau_NDVI)
    }
    
    #### Data Model: Phenocam_HistoricalData
    for(r in 2:num_dates){
      data$GCC[r] ~ dnorm(data$GCC[r],tau_GCC)
    }
    
    #### Process Model
    for(s in 2:num_dates){
      color[i,s] <- color[s-1] + growth*color[s-1]*(1-color[s-1]/1)
    }
    
    color[i,1] ~ dnorm(x_ic,tau_ic)
  }  ## end loop over individuals
  
  #### Priors
  tau_NDVI ~ dbeta(a_ndvi,r_ndvi)
  growth ~ dnorm(a_growth,r_growth)
  tau_GCC ~ dbeta(a_inc,r_inc)
  mu ~ dlnorm(mean_color,tau_color)
}
