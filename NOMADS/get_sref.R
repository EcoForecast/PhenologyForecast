### Script for getting SREF (Short range ensemble forecast) off the web
### translated from bash to R for greater control


### DEFINE FUNCTIONS FOR GRABBING GRIB DATA
grabvar <- function(vstring,vname){

  ## vstring - string searched for in grib file
  ## vname   - variable name for output
  
  ## check presence
  pres <- dir(dirname,paste(vname,'.',MOD,'.',MEM,'.',CYC,'.',TM,sep=""))
  
  ## grab
 if(length(pres) == 0){
   system(paste('grep "',vstring,'" < temp.inv.',MOD,' | ',NDIR,'/get_grib.pl ',fname,' ',dirname,'/',vname,'.',MOD,'.',MEM,'.',CYC,'.',TM,sep=""))
 }
}
grabmap <- function(mname){

  ## check presence
  pres <- dir(dirname,paste(mname,'.',MOD,sep=""))

  ##grab map variable
  if(length(pres) == 0){
    system(paste('grep ":',mname,'" < temp.inv.',MOD,' | ',NDIR,'/get_grib.pl     ',fname,' ',dirname,'/',mname,'.',MOD,sep=""))
  }
}

######################    START ######################################

## First, stop if another copy of this script is already running
np <- system("top -b -n 1 -u dietze | grep get",intern=TRUE)# 

print(np)

if(length(np) <= 1) {
  
  ## second, set up variables
  HTTP_SITE="http://nomad3.ncep.noaa.gov/pub/sref/sref"
  NDIR="/projectnb/cheas/gapmacro/ebi/NOMADS"
  DATE=system("date -u +%Y%m%d",intern=TRUE)
  CYCLE=c("21","03","09","15","21")
  CYC='03'
  ##MODEL=(em em em eta eta eta eta nmm nmm nmm rsm rsm rsm)
  ##MEMBER=(ctl n1 p1 ctl1 ctl2 n1 p1 ctl n1 p1 ctl1 n1 p1)
  MODEL=  c("em","eta","eta","nmm","rsm" )
  MEMBER= c("ctl","ctl1","ctl2","ctl","ctl1")
  TIME=c("00","03","06","09","12","15","18","21","24","27","30","33","36","39","42","45","48","51","54","57","60","63")
  
  ## select day and time, make output directory
  dirname=paste("/projectnb/cheas/gapmacro/ebi//NOMADS/SREF/",DATE,sep="")
  if (length(grep(DATE,dir("/projectnb/cheas/gapmacro/ebi/NOMADS/SREF/")))==0){
    print(paste(dirname,"does not exist"))
    system(paste("mkdir",dirname))
  }
   
  ## ADD CODE TO CHECK IF MOST RECENT CYCLE WAS PROCESSED AND QUIT IF ALREADY DONE
  HR=system("date -u +%H",intern=TRUE)
  hn=floor((as.numeric(HR)+2)/6)
  ##CYC=CYCLE[hn]  ## note:: need to actually set date back 1 if hn = 0
  print(paste(HR,hn,CYC))
  
  if(hn > 1){  ## check that most recent files are for today  
    ## loop over models and ensemble members
    for (i in 1:length(MODEL)){ 
      
      MOD=MODEL[i]
      MEM=MEMBER[i]      
      print(c(i,MOD,MEM))
      
      ## CHECK IF FILES EXIST
      fexist <- dir(dirname,paste(MOD,".",MEM,".",CYC,sep=""))
      if(length(fexist) >=196) next    ## all files present
      
      ## loop over TIME
      for(TM in TIME){
        
        ## set filename
        fname= paste(HTTP_SITE,DATE,"/sref_",MOD,".t",CYC,"z.pgrb212.",MEM,".f",TM,sep="")
        print(c("TIME",TM,fname))
        
        ## grab index
        system(paste(NDIR,"/get_inv.pl ",fname,".inv > temp.inv.",MOD,sep=""))
        
        ## see if index was present
        invpres <- length(dir(pattern=paste("temp.inv.",MOD,sep="")))
        if (invpres>0){
          
          ## grab data variables
          grabvar(":TMP:2 m",  "TMP2")
          grabvar(":TMP:sfc",  "TMPs")
          grabvar(":UGRD:10 m","UGRD")
          grabvar(":RH:2 m",   "RH")
          grabvar(":DLWRF:sfc","DLWR")
          grabvar(":DSWRF:sfc","DSWR")
          grabvar(":PRATE",    "PRATE")
          grabvar(":PRES:sfc", "PRES")
          grabvar(":CSNOW",    "CSNOW")
          grabvar(":CICEP",    "CICEP")
          grabvar(":CFRZR",    "CFRZR")
          grabvar(":CRAIN",    "CRAIN")
          grabvar(":HGT:s",    "HGT")
          
          ## GRAB SPATIAL VARIABLES
          if (TM == "00"){
            
            grabmap("ELON")
            grabmap("NLAT")
            grabmap("LAND")
          }
          
          ## clean-up
          paste("rm temp.inv.",MOD,sep="")
          
          
        }  ##END - inventory present
        else {
          ## echo "inventory for $MOD not present -- $fname.inv"
        }
        
      }  ##END - loop over forecast time

  
    }  ## END - loop over ensemble members

  } else { ## most recent file was yesterday
    paste("NEED TO IMPLEMENT LOOK BACK")
  }
  
  ## run script to build met drivers


} ## END - script already running
