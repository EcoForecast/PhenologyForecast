#!/bin/bash
# Make gif animation out of plots:

cd /home/dgianotti/doc/bu/classes/ge585/PhenologyForecast/pdfs;

for d in site*
do cd "$d";

for f in *.pdf; 
do 
convert ./"$f" ./"${f%.pdf}.jpg"; 
done

convert -delay 25 *.jpg -loop 0 Site1.gif;
#rm *.jpg;
cd /home/dgianotti/doc/bu/classes/ge585/PhenologyForecast/pdfs;
done
