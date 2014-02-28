## Code to convert most recent SREF files into met drivers and figures

##################################
##                              ##
##    SINGLE SITE TIMESERIES    ## 
##                              ##
##################################
## how to extract a specific cell
LAT <- 40.061187
LON <- 360-88.195496

NDIR <- "/home/scratch/dietze_lab/NOMADS"

## determine what the most recent files "should be"
date <- system("date -u +%Y%m%d",intern=TRUE)
dirname <- paste(NDIR,"SREF",date,sep="/")
hr <- system("date -u +%k",intern=TRUE)
CYC <- "03"

## check if files have been processed yet
hfiles <- dir(dirname,"h5")
if(length(hfiles)>0){
  hfiles <- hfiles[grep(CYC,hfiles)]
  if(length(hfiles)>0){ stop()}
}

## check if GRIB files are there yet
gfiles <- dir(dirname)
gfiles[grep("h5",gfiles)] <- NA
gfiles[grep("pdf",gfiles)] <- NA
gfiles <- gfiles[!is.na(gfiles)] ## remove non-grib files from list
if(length(gfiles) == 0) stop()
fsplit <- strsplit(gfiles, ".",fixed=TRUE)
npart  <- do.call("rbind",lapply(fsplit,length))
imap <- which(npart == 2)
imet <- which(npart == 5)
var <- mod <- mem <- stime <- ftime <- rep(NA,length(imet))
for(i in 1:length(imet)){
  j <- imet[i]
  var[i] <- fsplit[[j]][1]
  mod[i] <- fsplit[[j]][2]
  mem[i] <- fsplit[[j]][3]
  stime[i] <- fsplit[[j]][4]
  ftime[i] <- fsplit[[j]][5]
}
icyc <- which(stime == CYC)
imet <- imet[icyc]
if(length(icyc) == 0) stop()

METDATA <- list()

## loop over ensemble member
modmem <- paste(mod[icyc],mem[icyc],sep=".")
modmemU <- unique(modmem)
for(i in 1:length(modmemU)){
  
  ## load maps
  mapmod <- strsplit(modmemU[i],".",fixed=TRUE)[[1]][1]
  system(paste(NDIR,"/wgrib -d 1 ",dirname,"/NLAT.",mapmod," -text -h -o ",dirname,"/NLAT.",mapmod,".txt", sep=""))
  system(paste(NDIR,"/wgrib -d 1 ",dirname,"/ELON.",mapmod," -text -h -o ",dirname,"/ELON.",mapmod,".txt", sep=""))
  system(paste(NDIR,"/wgrib -d 1 ",dirname,"/LAND.",mapmod," -text -h -o ",dirname,"/LAND.",mapmod,".txt", sep=""))

  NLAT <- read.table(paste(dirname,"/NLAT.",mapmod,".txt",sep=""),skip=1,header=FALSE)
  ELON <- read.table(paste(dirname,"/ELON.",mapmod,".txt",sep=""),skip=1,header=FALSE)
  LAND <- read.table(paste(dirname,"/LAND.",mapmod,".txt",sep=""),skip=1,header=FALSE)

  NLAT[NLAT>1.0e20] <- NA
  ELON[ELON>1.0e20] <- NA
  LAND[LAND>1.0e20] <- NA
  NLAT <- NLAT[[1]]
  ELON <- ELON[[1]]
  LAND <- LAND[[1]]
## Determine extraction location
  ROW <- which.min((LAT-NLAT)^2 +(LON-ELON)^2)

### LOOP OVER TIME
  imm <- grep(modmemU,gfiles[imet])
  times <- unique(ftime[icyc[imm]])
  met <- matrix(NA,nrow=length(times),ncol=14)  
  for(it in 1:length(times)){
    
### OPEN AND READ
    
### FREEZING RAIN
    dfile <- paste("CFRZR.",modmemU[i],".",CYC,".",times[it],sep="")
    if(length(dir(dirname,dfile))>0){
      system(paste(NDIR,"/wgrib -d 1 ",dirname,"/",dfile," -text -h -o ",dirname,"/",dfile,".txt", sep=""))
      CFRZR <- read.table(paste(dirname,"/",dfile,".txt",sep=""),skip=1,header=FALSE)
      CFRZR <- CFRZR[[1]]
      if(length(CFRZR)> ROW){
        met[it,1] <- CFRZR[ROW]
      }
    }
    
    ## ICE PELLETS
    dfile <- paste("CICEP.",modmemU[i],".",CYC,".",times[it],sep="")
    if(length(dir(dirname,dfile))>0){
      system(paste(NDIR,"/wgrib -d 1 ",dirname,"/",dfile," -text -h -o ",dirname,"/",dfile,".txt", sep=""))
      VBLE <- read.table(paste(dirname,"/",dfile,".txt",sep=""),skip=1,header=FALSE)
      VBLE <- VBLE[[1]]
      if(length(VBLE)> ROW){
        met[it,2] <- VBLE[ROW]
      }
    }

    ## RAIN
    dfile <- paste("CRAIN.",modmemU[i],".",CYC,".",times[it],sep="")
    if(length(dir(dirname,dfile))>0){
      system(paste(NDIR,"/wgrib -d 1 ",dirname,"/",dfile," -text -h -o ",dirname,"/",dfile,".txt", sep=""))
      VBLE <- read.table(paste(dirname,"/",dfile,".txt",sep=""),skip=1,header=FALSE)
      VBLE <- VBLE[[1]]
      if(length(VBLE)> ROW){
        met[it,3] <- VBLE[ROW]
      }
    }

    ## SNOW
    dfile <- paste("CSNOW.",modmemU[i],".",CYC,".",times[it],sep="")
    if(length(dir(dirname,dfile))>0){
      system(paste(NDIR,"/wgrib -d 1 ",dirname,"/",dfile," -text -h -o ",dirname,"/",dfile,".txt", sep=""))
      VBLE <- read.table(paste(dirname,"/",dfile,".txt",sep=""),skip=1,header=FALSE)
      VBLE <- VBLE[[1]]
      if(length(VBLE)> ROW){
        met[it,4] <- VBLE[ROW]
      }
    }
    
    ## LONG-WAVE
    dfile <- paste("DLWR.",modmemU[i],".",CYC,".",times[it],sep="")
    if(length(dir(dirname,dfile))>0){
      system(paste(NDIR,"/wgrib -d 1 ",dirname,"/",dfile," -text -h -o ",dirname,"/",dfile,".txt", sep=""))
      VBLE <- read.table(paste(dirname,"/",dfile,".txt",sep=""),skip=1,header=FALSE)
      VBLE <- VBLE[[1]]
      if(length(VBLE)> ROW){
        met[it,5] <- VBLE[ROW]
      }
    }
    
    ## SHORT WAVE
    dfile <- paste("DSWR.",modmemU[i],".",CYC,".",times[it],sep="")
    if(length(dir(dirname,dfile))>0){
      system(paste(NDIR,"/wgrib -d 1 ",dirname,"/",dfile," -text -h -o ",dirname,"/",dfile,".txt", sep=""))
      VBLE <- read.table(paste(dirname,"/",dfile,".txt",sep=""),skip=1,header=FALSE)
      VBLE <- VBLE[[1]]
      if(length(VBLE)> ROW){
        met[it,6] <- VBLE[ROW]
      }
    }

    ## GEOPOTENTIAL HEIGHT
    dfile <- paste("HGT.",modmemU[i],".",CYC,".",times[it],sep="")
    if(length(dir(dirname,dfile))>0){
      system(paste(NDIR,"/wgrib -d 1 ",dirname,"/",dfile," -text -h -o ",dirname,"/",dfile,".txt", sep=""))
      VBLE <- read.table(paste(dirname,"/",dfile,".txt",sep=""),skip=1,header=FALSE)
      VBLE <- VBLE[[1]]
      if(length(VBLE)> ROW){
        met[it,7] <- VBLE[ROW]
      }
    }

     ## PRECIP
    dfile <- paste("PRATE.",modmemU[i],".",CYC,".",times[it],sep="")
    if(length(dir(dirname,dfile))>0){
      system(paste(NDIR,"/wgrib -d 1 ",dirname,"/",dfile," -text -h -o ",dirname,"/",dfile,".txt", sep=""))
      VBLE <- read.table(paste(dirname,"/",dfile,".txt",sep=""),skip=1,header=FALSE)
      VBLE <- VBLE[[1]]
      if(length(VBLE)> ROW){
        met[it,8] <- VBLE[ROW]
      }
    }

    ## PRESSURE
    dfile <- paste("PRES.",modmemU[i],".",CYC,".",times[it],sep="")
    if(length(dir(dirname,dfile))>0){
      system(paste(NDIR,"/wgrib -d 1 ",dirname,"/",dfile," -text -h -o ",dirname,"/",dfile,".txt", sep=""))
      VBLE <- read.table(paste(dirname,"/",dfile,".txt",sep=""),skip=1,header=FALSE)
      VBLE <- VBLE[[1]]
      if(length(VBLE)> ROW){
        met[it,9] <- VBLE[ROW]
      }
    }

    ## Relative Humidity
    dfile <- paste("RH.",modmemU[i],".",CYC,".",times[it],sep="")
    if(length(dir(dirname,dfile))>0){
      system(paste(NDIR,"/wgrib -d 1 ",dirname,"/",dfile," -text -h -o ",dirname,"/",dfile,".txt", sep=""))
      VBLE <- read.table(paste(dirname,"/",dfile,".txt",sep=""),skip=1,header=FALSE)
      VBLE <- VBLE[[1]]
      if(length(VBLE)> ROW){
        met[it,10] <- VBLE[ROW]
      }
    }
    
    ## TEMPERATURE - 2m
    dfile <- paste("TMP2.",modmemU[i],".",CYC,".",times[it],sep="")
    if(length(dir(dirname,dfile))>0){
      system(paste(NDIR,"/wgrib -d 1 ",dirname,"/",dfile," -text -h -o ",dirname,"/",dfile,".txt", sep=""))
      VBLE <- read.table(paste(dirname,"/",dfile,".txt",sep=""),skip=1,header=FALSE)
      VBLE <- VBLE[[1]]
      if(length(VBLE)> ROW){
        met[it,11] <- VBLE[ROW]
      }
    }
    
    ## TEMPERATURE - surface
    dfile <- paste("TMPs.",modmemU[i],".",CYC,".",times[it],sep="")
    if(length(dir(dirname,dfile))>0){
      system(paste(NDIR,"/wgrib -d 1 ",dirname,"/",dfile," -text -h -o ",dirname,"/",dfile,".txt", sep=""))
      VBLE <- read.table(paste(dirname,"/",dfile,".txt",sep=""),skip=1,header=FALSE)
      VBLE <- VBLE[[1]]
      if(length(VBLE)> ROW){
        met[it,12] <- VBLE[ROW]
      }
    }

    ## WIND (U)
    dfile <- paste("UGRD.",modmemU[i],".",CYC,".",times[it],sep="")
    if(length(dir(dirname,dfile))>0){
      system(paste(NDIR,"/wgrib -d 1 ",dirname,"/",dfile," -text -h -o ",dirname,"/",dfile,".txt", sep=""))
      VBLE <- read.table(paste(dirname,"/",dfile,".txt",sep=""),skip=1,header=FALSE)
      VBLE <- VBLE[[1]]
      if(length(VBLE)> ROW){
        met[it,13] <- VBLE[ROW]
      }
    }

    ## WIND (V)
    dfile <- paste("VGRD.",modmemU[i],".",CYC,".",times[it],sep="")
    if(length(dir(dirname,dfile))>0){
      system(paste(NDIR,"/wgrib -d 1 ",dirname,"/",dfile," -text -h -o ",dirname,"/",dfile,".txt", sep=""))
      VBLE <- read.table(paste(dirname,"/",dfile,".txt",sep=""),skip=1,header=FALSE)
      VBLE <- VBLE[[1]]
      if(length(VBLE)> ROW){
        met[it,14] <- VBLE[ROW]
      }
    }
    
  }  ## END LOOP OVER TIME
  METDATA[[i]] <- met
}  ## end loop over ensemble members

## clean up
system(paste("rm ",dirname,"/*.txt",sep=""))

### MAKE GRAPHS
nt <- nrow(METDATA[[1]])
tseq <- seq(0,length=nt,by=3)
ne <- length(METDATA)
vname <- sort(unique(var))
#par(ask=TRUE)
#pdf(paste(dirname,"/pred.",CYC,".pdf",sep=""),width=10.5,height=7.5)
par(mfrow=c(2,2))
for(i in 1:14){

  ens <- matrix(NA,nt,ne)
  for(j in 1:ne){
    ens[,j] <- METDATA[[j]][,i]
  }
  rng <- range(ens,na.rm=TRUE)
  ebar <- apply(ens,1,mean)
  if(sum(!is.na(ebar))>0){
    plot(tseq,tseq,type='n',ylim=rng,main=vname[i])
    for(j in 1:ne){
      lines(tseq,ens[,j],col=j+1)
    }
    lines(tseq,ebar,col=1,lwd=3)
  } else {
    print(vname[i])
  }

  if(i %in% c(11,12)){
    abline(h=seq(373.15,length=20,by=-10),lty=3,lwd=0.5)
  }
  
}

### EXPORT DATA TO HDF
library(hdf5,lib.loc="/home/mdietze/lib/R/Rhdf")
for(i in 1:ne){

  ## set up time variables

  ## convert units
  n     <- length(Tair)
  nbdsfA <- (SW - SWd) * 0.57 # near IR beam downward solar radiation [W/m2]
  nddsfA <- SWd * 0.48        # near IR diffuse downward solar radiation [W/m2]
  vbdsfA <- (SW - SWd) * 0.43 # visible beam downward solar radiation [W/m2]
  vddsfA <- SWd * 0.52        # visible diffuse downward solar radiation [W/m2]
  prateA <- Rain              # precipitation rate [kg_H2O/m2/s]
  dlwrfA <- LW                # downward long wave radiation [W/m2]
  presA  <- pres              # pressure [Pa]
  hgtA   <- rep(50,n)         # geopotential height [m]
  ugrdA  <- Wind              # zonal wind [m/s]
  vgrdA  <- rep(0,n)          # meridional wind [m/s]
  shA    <- Qair              # specific humidity [kg_H2O/kg_air]
  tmpA   <- Tair              # temperature [K]
  co2A   <- CO2               # surface co2 concentration [ppm]
  
  ## write
  mout <- paste(froot,"/",froot,"_",y,month[m],".h5",sep="")
  dims <- c(1,1,length(selm))
  nbdsf <- array(nbdsfA[selm],dim=dims)
  nddsf <- array(nddsfA[selm],dim=dims)
  vbdsf <- array(vbdsfA[selm],dim=dims)
  vddsf <- array(vddsfA[selm],dim=dims)
  prate <- array(prateA[selm],dim=dims)
  dlwrf <- array(dlwrfA[selm],dim=dims)
  pres  <- array(presA[selm],dim=dims)
  hgt   <- array(hgtA[selm],dim=dims)
  ugrd  <- array(ugrdA[selm],dim=dims)
  vgrd  <- array(vgrdA[selm],dim=dims)
  sh    <- array(shA[selm],dim=dims)
  tmp   <- array(tmpA[selm],dim=dims)
#  co2   <- array(co2A[selm],dim=dims)
  hdf5save(mout,"nbdsf","nddsf","vbdsf","vddsf","prate","dlwrf","pres","hgt","ugrd","vgrd","sh","tmp","co2")
  
}
##################################
##                              ##
##    SINGLE SITE TIMESERIES    ## 
##                              ##
##################################
## how to extract a specific cell
LAT <- 40.061187
LON <- 360-88.195496

## un-tar
## loop through files
## grab variables
## put into monthly file
## clean up


##################################
##                              ##
##      SPATIAL TIMESERIES      ## 
##                              ##
##################################
## 
##REGION
if(false){
XLL <- 360-98
YLL <- 25
DX <- DY <- 1
NX <- 32
NY <- 50

glat <- floor(NLAT)
glon <- floor(ELON)
  
## loop over months
## set up storage
## un-tar
## loop through files
## grab variables
## put into monthly file
## clean up
}
