# SoilMoisture

# Constact info
- Radost Stanimirova: rkstan@bu.edu
- Eric Bullock: bullocke@bu.edu
- Paulo Arevalo: parevalo@bu.edu 
- Chi Chen:chenchi@bu.edu
- Tim Condon: condontd@bu.edu

# AWS server info 
- Public IP : 52.27.126.137
- Directory of our SoilMoisture project: /home/carya/SoilMoisture
- Our website: 52.27.126.137/SoilMoisture
- SSH to AWS: ssh -i ecoforecast.pem.txt carya@52.27.126.137  (change the pem Pair key to your own path!)

# Cron settings 
- SHELL=/bin/bash
- PATH=/usr/local/bin:/usr/bin:/bin:/usr/local/sbin:/usr/sbin:/sbin
- HOME=/home/carya/SoilMoisture
- 0 0 * * * cron_job.sh

