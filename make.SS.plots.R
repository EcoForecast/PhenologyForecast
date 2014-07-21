make.SS.plots <- function(out,time,
                          rescaled_NDVI,rescaled_GCC,site.number){
  source("ciEnvelope.R")
  
  time_year = as.numeric(format(as.Date(time), "%Y"))
  current.year = as.numeric(strftime(Sys.Date(),"%Y"))
  years <- unique(time_year)
  years <- years[years != current.year]
 
  plot_file_name = paste('Jags.SS.out.site',as.character(site.number), 'pdf',sep=".")
  
  pdf(plot_file_name)
  

  for(count in 1:length(years)){
    YR = years[count]
    
    II = which(time_year== YR)
    rescaled_NDVI_one_year = rescaled_NDVI[II]  # get ndvi just for ONE year
    rescaled_GCC_one_year = rescaled_GCC[II]    # get gcc just for ONE year
    
    # delete leap days 
    if (length(II) == 185){
      rescaled_NDVI_one_year = rescaled_NDVI_one_year[1:184]
      rescaled_GCC_one_year = rescaled_GCC_one_year[1:184]
    }
    
    # NDVI and GCC
    plot(182:365,out$ci[count,,2],type='l',ylim=c(0, 1),main=paste("SS model", as.character(YR)),
         ylab="Rescaled NDVI, GCC",xlab="DOY")
    ciEnvelope(182:365,out$ci[count,,1],out$ci[count,,3],col="lightBlue")
#    if(!is.null(dim(rescaled_NDVI_one_year))){ # R is stupid with NAs...
      points(182:365,rescaled_NDVI_one_year,pch="+",cex=0.8)
#    }
#    if(!is.null(dim(rescaled_GCC_one_year))){ # R is stupid with NAs...
      points(182:365,rescaled_GCC_one_year,pch="o",cex=0.5)
#    }
    lines(182:365,out$ci[count,,2],type='l',lwd=2)
    
    }
  
  ##### MCMC diagnostics
  source("corr.and.MCMC.Diag.forSSModel.R")
  #corr.and.MCMC.Diag.forSSModel(out)
  pairs(out$parms[sample.int(nrow(out$parms),2500),],pch=".")
  
  dev.off()
 
}