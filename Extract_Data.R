##Function for retrieving data and outputting as a CSV. 

library(raster, quietly=TRUE,warn.conflicts=FALSE) 
library(gdalUtils, quietly=TRUE,warn.conflicts=FALSE)
library(rgdal, quietly=TRUE,warn.conflicts=FALSE)
library(optparse, quietly=TRUE,warn.conflicts=FALSE)
library(rasterVis, quietly=TRUE,warn.conflicts=FALSE)
library(sp, quietly=TRUE,warn.conflicts=FALSE)

suppressPackageStartupMessages(library("optparse"))
option_list = list(
  make_option(c("-i", "--input"), action="store", default=NA, type='character', 
              help="Path to input file")
)
opt = parse_args(OptionParser(option_list=option_list))


if (length(opt$input)==0) {
  stop("No files to process", call.=FALSE)
} 

input <- opt$input


get_ndvi <- function(roi, image){
  ###Function to get today's NDVI at point of interest. 
  
  #Inputs:
    #roi: Point of interest. Format: SpatialPoints
    #image: MODIS surface reflectance image. Format: .hdf file 
  
  #Outputs:
    #NDVI: NDVI value at roi. Format: integer
    #Today: Today's date. Format: Date. 
  
  #Get today's date for the temp files. 
  today <- Sys.Date()
  #Get subdatasets from the HDF file. 
  subdatasets <- get_subdatasets(image)
  red <- grep('b01',subdatasets)
  nir <- grep('b02', subdatasets)
  redband <- subdatasets[red]
  nirband <- subdatasets[nir]

  #Try using hdf4 bindings to directly read in data. 
  redraster <- try(raster(redband))
  nirraster <- try(raster(nirband))
  msg <- geterrmessage()
  if (grepl("Cannot create a RasterLayer", msg)) {
    print('Using alternative method to read in data')
    outred <- paste(today,'_red.tif',sep='')
    outnir <- paste(today,'_nir.tif',sep='')
    gdal_translate(redband, outred, of = 'GTiff')
    gdal_translate(nirband, outnir, of = 'GTiff')
    redraster <- raster(outred)
    nirraster <- raster(outnir)
  }  
  msg <- {}
  
  #Calculate NDVI
  ndvi <- (nirraster - redraster) / (redraster + nirraster)
  ndvi <- raster(ndvi)
  
  #Read in ROI data and project to the MODIS Sinusoidal Projection
  projection(roi) <- CRS("+proj=lonlat +ellps=WGS84")
  roi_rep <- spTransform(roi, CRS(projection(ndvi)))
  ndvi_value <- extract(ndvi,roi_rep)
  return(ndvi_value)
}

get_soil_moisture <- function(roi,image) {
  
  ###Function to get today's soil moisture value at point of interest. 
  
  #Inputs:
    #roi: Point of interest. Format: SpatialPoints
    #image: SMAP surface moisture image. Format: .hdf file 
  
  #Outputs:
    #soil_m: Soil moisture value at roi. Format: integer
    #Today: Today's date. Format: Date. 
  
  today <- Sys.Date()
  
  #Get subdatasets from the HDF file. 
  subdatasets <- get_subdatasets('SMAP_L3_SM_P_20151126_R12170_001.h5')
  soilmoisture <- grep('soil_moisture',subdatasets)
  smband <- subdatasets[soilmoisture[1]]

  
  #Try using hdf4 bindings to directly read in data. 
  soilmoistureraster <- try(raster(smband))


  msg <- geterrmessage()
  if (grepl("Cannot create a RasterLayer", msg)) {
    print('Using alternative method to read in data')
    outsm <- paste(today,'_sm.tif',sep='')
    latsm <- paste(today,'_lat.tif',sep='')
    longsm <- paste(today,'_long.tif',sep='')
    gdal_translate(smband, outsm, of = 'GTiff')
    soilmoistureraster <- raster(outsm)
  }  
  msg <- {}
  
  #Set projection of ROI
  projection(roi) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")


  #Set EASE2 Grid projection for SMAP data
  proj4string(soilmoistureraster) <- CRS("+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs ")

  
  #Realign and reproject EASE grid raster onto WGS84 Lat/Long grid
  ex=extent(-17327926.6562500000000000,17340460.418750010430812,-7338517.9437500033527613,7338517.9437499996274710)
  sm_r <- setExtent(soilmoistureraster,ex)



  #Reproject ROI to projection of SMAP data
  roi_rep <- spTransform(roi, CRS(projection(soilmoistureraster)))
  sm_r <- setExtent(soilmoistureraster,ex)

  soil_m <- extract(sm_r,roi_rep)

  return(soil_m)
}

get_GPM<- function(roi,image) {
  
  ###Function to get today's rainfall value at point of interest. 
  
  #Inputs:
  #roi: Point of interest. Format: SpatialPoints
  #image: SMAP surface moisture image. Format: .hdf file 
  
  #Outputs:
  #rainfall: Rainfall value at roi. Format: integer
  #Today: Today's date. Format: Date. 
  
  today <- Sys.Date()

  #Open the image
  rainfallraster <- try(raster(image))
  msg <- geterrmessage()
  if (grepl("Cannot create a RasterLayer", msg)) {
    print('File is missing')
  }  
  msg <- {}
  
  #Assign the projection and extents
  projection(roi) <- CRS("+proj=lonlat +ellps=WGS84")
  projection(rainfallraster) <- CRS("+proj=lonlat +ellps=WGS84")
  ex=extent(-180, 180, -90, 90)
  rain_ext <- setExtent(rainfallraster,ex)

  
  #Retrieve coordinates of point and extract data at point. 
  rain  <- extract(rain_ext,roi)

  return(soil_m)
}

write_csv <- function(today, value, data) {
  csv=paste(data,'.csv',sep='')
  r_csv <- read.csv(csv,sep=',',stringsAsFactors=FALSE,header=TRUE)
  Date<-as.character(today)
  Data<-as.character(value)
  out_df <- data.frame(Date,Data)
  out_csv <- rbind(r_csv,out_df)
  write.csv(out_csv,csv)
}

#Start actual code



#Define region of interest
long=-98.01
lat=29.93
coords <- as.data.frame(cbind(long, lat))
roi <- SpatialPoints(coords)

#Today's date
today <- Sys.Date()



in_name <- substr(input,0,3)

if (in_name == "MOD") {
    value <- get_ndvi(roi, input)
    write_csv(today, value, 'MODIS')
  } else if (in_name =="GPM") {
    data <- get_GPM(roi, input)
    write_csv(today, value, 'GPM')
  } else if (in_name == "SMA") {
    data <- get_soil_moisture(roi, input)
    write_csv(today, value, 'SMAP')
} else {
  print('ERROR: Unrecognized file format')
}


