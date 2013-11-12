#!/bin/bash

R --vanilla < Get_Historical_Data.R > RunPhenologyProject.log
R --vanilla < NDVI_PhenoCam_DataFormat.R >> RunPhenologyProject.log
