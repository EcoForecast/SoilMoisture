#!/bin/bash

#Job name
#$ -N cronjob


echo `date`
echo `date` >> /home/carya/SoilMoisture/process_log.txt
# Run download job
cd /home/carya/SoilMoisture/
python 01_download_data.py "parevalo@bu.edu" "parevalo@bu.edu"


#Compare MODIS data to process log, and process any new data.
for i in data/MODIS/MY*hdf; do
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
for i in data/GPM/3*1day.tif; do
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

Rscript 04_Data_ReOrganizing.R

Rscript 05_soil_moisture_model_v003.R

Rscript 06_particle_filter_v001.R

convert -delay 50 -loop 0 example/gif/Forecast_PF_plot*.png web/plots/animatePF.gif 
