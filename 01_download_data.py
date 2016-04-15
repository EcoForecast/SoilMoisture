#!/usr/bin/python

import csv
import forecastio
import sys
import ftplib
from ftplib import FTP
import os
import datetime as dt

"""
This script automatically logs in to the ftp servers of the Near Real Time (NRT) GPM products, as well as the SMAP
and MODIS products. For GPM, given the messy structure of the data folder (NRTPUB/imerg/gis/), two different sections
were required: one for current year, with the data stored in subfolders in the parent directory, and another for older
years, which have their own year folder and subfolders. For SMAP and MODIS the functions are a more standardized and
could potentially be turned into a single reusable function in the future. Also, another library could be used in
order to streamline the process (http://ftputil.sschwarzer.net/trac). Eventually, some of the simpler cases like MODIS
could be done just by using WGET.

Args:
	username (str): Email address (must be registered in GPM server to work for that product)
	password (str): Same email address


"""

# Get username and password for the ftp servers
usern = sys.argv[1]
passw = sys.argv[2]


def ftp_connect(host, username, password, directory):
    """Connects to the specified FTP and cd's to the specified parent directory, where all subfolders and files
    of interest are stored"""
    ftp = FTP(host)
    ftp.login(username, password)
    ftp.cwd(directory)

    return ftp


def download_files(ftp_con, sub):
    """Finds the files in the current folder that meet the required criteria, e.g.
        last calculated daily product. The substring (sub variable) makes reference
         to the string we want to look for in the filenames, e.g. product type or
         tile information"""

    # Find the files we need to download
    files = ftp_con.nlst()  # Get files in the folder
    req_files = list(s for s in files if sub in s)  # Retrieve required files only

    # Download the files
    for f in range(len(req_files)):
        if os.path.isfile(req_files[f]):
            pass
        else:
            retr_command = "RETR " + req_files[f]
            ftp_con.retrbinary(retr_command, open(req_files[f], 'wb').write)

# Get current date information
now = dt.datetime.now()
year = int(now.strftime('%Y'))
month = int(now.strftime('%m'))
day = int(now.strftime('%d'))

# Connect to GPM and download files
# Set working directory
#os.chdir("C:/test/GPM")
os.chdir("/home/carya/SoilMoisture/data/GPM")
ftp = ftp_connect("jsimpson.pps.eosdis.nasa.gov", usern, passw, "NRTPUB/imerg/gis/")

# Connect to ftp server and get list of files for GPM data
# For current year

for i in range(month):
    m = str(i + 1).zfill(2)
    ftp.cwd(m)
    download_files(ftp, "1410.V03E.1day.t")
    ftp.cwd('..')  # We ned to go back to previous level

# For older years
for y in range(2015, year):  # Data starts in 2015
    ftp.cwd(str(y))
    for i in range(12):  # Month folders inside each year
        m = str(i + 1).zfill(2)
        try:
            ftp.cwd(m)
            download_files(ftp, "1410.V03E.1day.t")
            ftp.cwd('..')
            print ftp.pwd()
        except ftplib.error_perm, resp:
            if str(resp) == "550 {0}/{1}: No such file or directory".format(y, m):
                print "No folders for this date:{0}-{1}".format(y, m)

    ftp.cwd('..')

ftp.quit()

# Connect to SMAP server and download files
# Set working directory
#os.chdir("C:/test/SMAP")
os.chdir("/home/carya/SoilMoisture/data/SMAP")
ftp = ftp_connect("n5eil01u.ecs.nsidc.org", "anonymous", passw, "/SAN/SMAP/SPL3SMP.002/")
ftp.dir()
ls = []
ftp.retrlines('MLSD', ls.append) # Retrieve files and folders from the server folder
list_folders = list(s for s in ls if "=dir" in s)  # Filter folders only

# Super ugly but does the job
folder_names = []
for fd in range(len(list_folders)):
    folder_names.append(list_folders[fd].split("; ")[1])  # Iterate over list, split string, retrieve second element

# Iterate over folder names, download all the files
for f in folder_names:
    ftp.cwd(str(f))
    download_files(ftp, "")
    ftp.cwd('..')

ftp.quit()

# Connect to MODIS server and downlaod files
# Set working directory
#os.chdir("C:/test/MODIS")
os.chdir("/home/carya/SoilMoisture/data/MODIS")

def get_modis(product, sub, start_year):
    """This function downloads all the files for a given MODIS product starting from the specified year until the
        current date that contain the specified substring (e.g h09v06)

     Args:
        product (str): Name of the MODIS product
        sub (str): Substring to look for in each filename
        start_year (int): Initial year of the period for which files will be downloaded

     """
    # Connect and go to the product folder
    ftp = ftp_connect("ladsweb.nascom.nasa.gov", "anonymous", passw, "/allData/6/" + product)

    # Iterate over folder names, download all the files
    for y in range(start_year, year + 1):  # Getting data from 2015 until the current date
        ftp.cwd(str(y))
        print ftp.pwd()
        day_list = ftp.nlst()  # Get day folder list
        for d in day_list:
            ftp.cwd(str(d))
            download_files(ftp, sub)
            ftp.cwd('..')
        ftp.pwd()
        ftp.cwd('..')

    ftp.quit()

get_modis("MOD09GA", "h09v06", 2015)
get_modis("MYD09GA", "h09v06", 2015)

#Get rain forecast from Forecast.io
api_key = "bc6ba93a1079777ad112ee263138b8bd"
lat = 29.93
lng = -98.01
forecast = forecastio.load_forecast(api_key, lat, lng)
byday = forecast.daily()
precipProbability=[]
precipIntensity=[]
#First day is today at 5 AM, every day after is forecst for that morning.
for r in byday.data:
    precipIntensity.append(r.precipIntensity)
    precipProbability.append(r.precipProbability)
rain={}
rain['probability'] = precipProbability
rain['intensity'] = precipIntensity

#Now write to csv
with open('rainforecast.csv', 'wb') as f:
    w = csv.writer(f)
    w.writerow(rain.keys())
    for i in range(len(rain['probability'])):
        w.writerow([rain[k][i] for k in rain.keys()])

