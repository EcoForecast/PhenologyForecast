#!/bin/bash
# Make gif animation out of plots:

cd /var/www/ge585/PhenologyForecast/pdfs;

for d in site*
do cd "$d";

for f in *.pdf; 
do 
convert ./"$f" ./"${f%.pdf}.jpg"; 
done

convert -delay 15 *.jpg -loop 0 Site"${d%.pdf}.gif";
rm *.jpg;
cd /var/www/ge585/PhenologyForecast/pdfs;
echo "Finished another site"
done
