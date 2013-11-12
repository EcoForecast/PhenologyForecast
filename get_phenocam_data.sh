#!/bin/sh

wget http://phenocam.sr.unh.edu/data/archive/howland1/ROI/howland1_canopy_0001_gcc.csv

wget http://phenocam.sr.unh.edu/data/archive/bartlett/ROI/bartlett_deciduous_0001_gcc.csv

wget http://phenocam.sr.unh.edu/data/archive/shalehillsczo/ROI/shalehillsczo_canopy_0001_gcc.csv

wget http://phenocam.sr.unh.edu/data/archive/shenandoah/ROI/shenandoah_canopy_0001_gcc.csv

wget http://phenocam.sr.unh.edu/data/archive/coweeta/ROI/coweeta_deciduous_0002_gcc.csv

mv howland1_canopy_0001_gcc.csv /var/www/ge585/
mv bartlett_deciduous_0001_gcc.csv /var/www/ge585/
mv shalehillsczo_canopy_0001_gcc.csv /var/www/ge585/
mv shenandoah_canopy_0001_gcc.csv /var/www/ge585/
mv coweeta_deciduous_0002_gcc.csv /var/www/ge585/

R CMD BATCH Phenolgy_MODIS.R