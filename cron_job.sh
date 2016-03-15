#!/bin/bash

#Job name
#$ -N cronjob






#Compare MODIS data to process log, and process any new data.
for i in data/MODIS/M*hdf; do
echo $i
if grep -Fxq $i process_log.txt
then
    continue   
else
    echo processing $i
    Rscript 02_Extract_Data.R -i $i 
    echo $i >> process_log.txt 
fi
done

#Compare GPM data to process log, and process any new data.
for i in data/GPM/3*tif; do
echo $i
if grep -Fxq $i process_log.txt
then
    continue   
else
    Rscript 02_Extract_Data.R -i $i 
    echo $i >> process_log.txt 
fi
done

#Compare SMAP data to process log, and process any new data.

for i in data/SMAP/SM*5; do
echo $i
if grep -Fxq $i process_log.txt
then
    continue   
else
    Rscript 02_Extract_Data.R -i $i 
    echo $i >> process_log.txt 
fi
done

Rscript 03_plot_time_series.R
