#!/bin/bash
# Make gif animation out of plots:
## arg 1 = model
## arg 2 = year

cd pdfs;

for d in site*
do cd "$d";

for f in *${1}*${2}*.pdf; 
do 
convert -size 1000x1000 ./"$f" ./"${f%.pdf}.jpg"; 
done

convert -delay 15 *.jpg -loop 0 Site"${d%.pdf}.${1}.${2}.gif";
rm *.jpg;
cd ..;
echo "Finished another site"
done
