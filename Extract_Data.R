##Function for retrieving data and outputting as a CSV. 

library(raster) 
library(gdalUtils)
library(rgdal)
library(MODIS) 

setwd('/Users/ericbullock/Google Drive/Class/Ecological_Forecasting/Project')

get_ndvi <- function(point, image){
  #Get today's date for the temp files. 
  today <- Sys.Date()
  
  #Get subdatasets from the HDF file. 
  hesubdatasets <- get_subdatasets(image)
  red <- grep('b03',subdatasets)
  nir <- grep('b04', subdatasets)
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
    gdal_translate(redband, outnir, of = 'GTiff')
    redraster <- raster(outred)
    nirraster <- raster(outnir)
  }  
    
  ndvi <- (nirraster - redraster) / (redraster + nirraster)
  ndvi <- raster(ndvi)
  roi <- readOGR('Point.shp','Point')
  
  ndvi_value <- extract(redraster,roi)
  
  return(ndvi_value)
}

get_soil_moisture <- function(point,image) {

  today <- Sys.Date()
  
  #Get subdatasets from the HDF file. 
  subdatasets <- get_subdatasets('smau.h2')
  soilmoisture <- grep('sm_profile_analysis',subdatasets)
  smband <- subdatasets[soilmoisture[1]]
  
  #Try using hdf4 bindings to directly read in data. 
  soilmoistureraster <- try(raster(smband))
  msg <- geterrmessage()
  if (grepl("Cannot create a RasterLayer", msg)) {
    print('Using alternative method to read in data')
    outsm <- paste(today,'_sm.tif',sep='')
    outnir <- paste(today,'_nir.tif',sep='')
    gdal_translate(redband, outsm, of = 'GTiff')
    soilmoistureraster <- raster(outsm)
  }  
}

