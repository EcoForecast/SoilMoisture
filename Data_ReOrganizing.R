#Written Chi, 03/27/2016
# 1. this script is going to re-organize our data by date. This is a temporary code to fix the data organizing issues.
# 2. It will take the mean if there are more than one daily observations
# 3. Combine multiple data sources. The starting day of the output will be the first day that we have soilmoisture data.
# 4. The object "combined" will be the final output. Check it and you will understand it.
# 5. Copy this code to your code

data.root.path = '/Users/chichen/Desktop/'
SMAP <- read.csv(sprintf("%sSMAP.csv",data.root.path))    ## read in soil moisture data 
GPM <- read.csv(sprintf("%sGPM.csv",data.root.path))      ## read in precipitation data 
MODIS <- read.csv(sprintf("%sMODIS.csv",data.root.path))    ## read in MODIS data 

preprocess.Data <- function(x){ 
  x=x[with(x,order(x$Date)),]
  value = tapply(x$Data,x$Date,mean,na.rm=TRUE)
  out = data.frame(Date=rownames(value),Data=value)
  rownames(out)=as.character(seq(1,length(value)))
  return(out)
}
MODIS=preprocess.Data(MODIS)
GPM = preprocess.Data(GPM)
SMAP = preprocess.Data(SMAP)


Date.Start = min(as.numeric(as.Date(GPM[,1])),as.numeric(as.Date(MODIS[,1])),as.numeric(as.Date(SMAP[,1])))
Date.End = max(as.numeric(as.Date(GPM[,1])),as.numeric(as.Date(MODIS[,1])),as.numeric(as.Date(SMAP[,1])))
combined = data.frame(Date=as.Date(character()),
                      NDVI=numeric(), 
                      Precip=numeric(),  
                      SoilMoisture=numeric()) 
n=0
for (idate in Date.Start:Date.End){
  n=n+1
  i_this_day = as.Date(idate,origin="1970-01-01")
  combined[n,1]=i_this_day
  ndvi = MODIS[as.Date(MODIS$Date)==i_this_day,2]
  if (is.null(ndvi)|length(ndvi)==0) {
    ndvi=NA}
  
  precip = GPM[as.Date(GPM$Date)==i_this_day,2]
  if (is.null(precip)|length(precip)==0) {
    precip=NA}
  
  smap = SMAP[as.Date(SMAP $Date)==i_this_day,2]
  if (is.null(smap)|length(smap)==0) {
    smap=NA}
  
  combined[n,2]=ndvi
  combined[n,3]=precip
  combined[n,4]=smap
}
NonNAindex <- min(which(!is.na(combined$SoilMoisture)))-1
combined=combined[-(1:NonNAindex),]






