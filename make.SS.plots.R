make.SS.plots <- function(jags.out.all.years.array,time,
                          rescaled_NDVI,rescaled_GCC,site.number){
  source("ciEnvelope.R")
  
  time_year = as.numeric(format(as.Date(time), "%Y"))
  current.year = as.numeric(strftime(Sys.Date(),"%Y"))
  years <- unique(time_year)
  years <- years[years != current.year]
 
  plot_file_name = paste('Jags.SS.out.site',as.character(site.number), 'pdf',sep=".")
  
  pdf(plot_file_name)
  
  count = 0
  for(YR in years){
    
    count = count+1
    jags.out.one.year = jags.out.all.years.array[,,count]

    II = which(time_year== YR)
    rescaled_NDVI_one_year = rescaled_NDVI[II]  # get ndvi just for ONE year
    rescaled_GCC_one_year = rescaled_GCC[II]    # get gcc just for ONE year
    
    # delete leap days 
    if (length(II) == 185){
      rescaled_NDVI_one_year = rescaled_NDVI_one_year[1:184]
      rescaled_GCC_one_year = rescaled_GCC_one_year[1:184]
    }
    
    
    
    # [r  tau_add  tau_gcc	tau_ndvi	x]
    ci <- apply((jags.out.one.year[,5:188]),2,quantile,c(0.025,0.5,0.975))
    
    # NDVI and GCC
    plot(182:365,ci[2,],type='l',ylim=c(0, 1),main=paste("SS model", as.character(YR)),
         ylab="Rescaled NDVI, GCC",xlab="DOY")
    ciEnvelope(182:365,ci[1,],ci[3,],col="lightBlue")
#    if(!is.null(dim(rescaled_NDVI_one_year))){ # R is stupid with NAs...
      points(182:365,rescaled_NDVI_one_year,pch="+",cex=0.8)
#    }
#    if(!is.null(dim(rescaled_GCC_one_year))){ # R is stupid with NAs...
      points(182:365,rescaled_GCC_one_year,pch="o",cex=0.5)
#    }
    lines(182:365,ci[2,],type='l',ylim=c(0, 1))
    
    
  }
  dev.off()
 
}