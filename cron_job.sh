#!/bin/bash

#Job name
#$ -N cronjob







#Find today's images and extract MODIS data

for i in data/MODIS/M*hdf; do
echo $i
if grep -Fxq $i process_log.txt
then
    continue   
else
    echo processing $i
    Rscript Extract_Data.R -i $i 
    echo $i >> process_log.txt 
fi
done

#Find and extract GPM data
for i in data/GPM/3*tif; do
echo $i
if grep -Fxq $i process_log.txt
then
    continue   
else
    Rscript Extract_Data.R -i $i 
    echo $i >> process_log.txt 
fi
done

#Find and extract Soil Moisture data
#img=$(find data/SMAP/ -name '*h5')

for i in data/SMAP/SM*5; do
echo $i
if grep -Fxq $i process_log.txt
then
    continue   
else
    Rscript Extract_Data.R -i $i 
    echo $i >> process_log.txt 
fi
done

