##Function for retrieving data and outputting as a CSV. 

repos = "http://cran.us.r-project.org"
get.pkg <- function(pkg){
  loaded <- do.call("require",list(package=pkg))
  if(!loaded){
    print(paste("trying to install",pkg))
    install.packages(pkg,dependencies=TRUE,repos=repos)
    loaded <- do.call("require",list(package=pkg))
    if(loaded){
      print(paste(pkg,"installed and loaded"))
    } 
    else {
      stop(paste("could not install",pkg))
    }    
  }
}
get.pkg("raster")
get.pkg("gdalUtils")
get.pkg("rgdal")
get.pkg("sp")
get.pkg("optparse")

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


get_ndvi <- function(roi, image, day){
  ###Function to get today's NDVI at point of interest. 
  
  #Inputs:
    #roi: Point of interest. Format: SpatialPoints
    #image: MODIS surface reflectance image. Format: .hdf file 
  
  #Outputs:
    #NDVI: NDVI value at roi. Format: integer
    #Today: Today's date. Format: Date. 

  
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
    outred <- paste('data/MODIS/',day,'_red.tif',sep='')
    outnir <- paste('data/MODIS/',day,'_nir.tif',sep='')
    gdal_translate(redband, outred, of = 'GTiff')
    gdal_translate(nirband, outnir, of = 'GTiff')
    redraster <- raster(outred)
    nirraster <- raster(outnir)
  }  
  msg <- {}
  
  #Calculate NDVI
  ndvi <- (nirraster - redraster) / (redraster + nirraster)

  
  #Read in ROI data and project to the MODIS Sinusoidal Projection
  projection(roi) <- CRS("+proj=lonlat +ellps=WGS84")
  roi_rep <- spTransform(roi, CRS(projection(ndvi)))
  ndvi_value <- extract(ndvi,roi_rep)
  return(ndvi_value)
}

get_soil_moisture <- function(roi,image, day) {
  
  ###Function to get today's soil moisture value at point of interest. 
  
  #Inputs:
    #roi: Point of interest. Format: SpatialPoints
    #image: SMAP surface moisture image. Format: .hdf file 
  
  #Outputs:
    #soil_m: Soil moisture value at roi. Format: integer
    #Today: Today's date. Format: Date. 
  

  #Get subdatasets from the HDF file. 
  subdatasets <- get_subdatasets(image)
  soilmoisture <- grep('soil_moisture',subdatasets)
  smband <- subdatasets[soilmoisture[1]]

  
  #Try using hdf4 bindings to directly read in data. 
  soilmoistureraster <- try(raster(smband))


  msg <- geterrmessage()
  if (grepl("Cannot create a RasterLayer", msg)) {
    print('Using alternative method to read in data')
    outsm <- paste('data/MODIS/',day,'_sm.tif',sep='')
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

get_GPM<- function(roi,image, day) {
  
  ###Function to get today's rainfall value at point of interest. 
  
  #Inputs:
  #roi: Point of interest. Format: SpatialPoints
  #image: SMAP surface moisture image. Format: .hdf file 
  
  #Outputs:
  #rainfall: Rainfall value at roi. Format: integer
  #Today: Today's date. Format: Date. 

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

  return(rain)
}

write_csv <- function(today, value, data) {
  csv=paste(data,'.csv',sep='')
  r_csv <- read.csv(csv,sep=',',stringsAsFactors=FALSE,header=TRUE)
  Date<-as.character(today)
  Data<-as.character(value)
  out_df <- data.frame(Date,Data)
  print(r_csv)
  print(out_df)
  out_csv <- rbind(r_csv,out_df)
  write.csv(out_csv,csv,row.names=FALSE)
}

#Start actual code



#Define region of interest
long=-98.01
lat=29.93
coords <- as.data.frame(cbind(long, lat))
roi <- SpatialPoints(coords)





in_name <- substr(input,6,8)

if (in_name == "MOD") {
    date_unformatted <- as.integer(substr(input,21,27))
    Date <- strptime(date_unformatted, "%Y%j")
    value <- get_ndvi(roi, input, date_unformatted)
    write_csv(Date, value, 'MODIS')
  } else if (in_name =="GPM") {
    date_unformatted <- as.integer(substr(input,33,40))
    Date <- strptime(date_unformatted, "%Y%m%d")
    value <- get_GPM(roi, input, date_unformatted)
    write_csv(Date, value, 'GPM')
  } else if (in_name == "SMA") {
    date_unformatted <- as.integer(substr(input,24,31))
    Date <- strptime(date_unformatted, "%Y%m%d")
    value <- get_soil_moisture(roi, input, date_unformatted)
    write_csv(Date, value, 'SMAP')
} else {
  print('ERROR: Unrecognized file format')
}


