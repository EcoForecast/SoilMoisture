#Written by Chi, 03/27/2016
# 1. This script is going to re-organize our data by date. 
# 2. It will take the mean if there are more than one daily observations
# 3. Combine multiple data sources. In the output file, the starting date will be the first day that we have soil moisture data.
# 4. The object "combined" is the final output. Check it and you will understand it.

data.root.path = './example/'
SMAP <- read.csv(sprintf("%sSMAP.csv",data.root.path))    ## read in soil moisture data 
GPM <- read.csv(sprintf("%sGPM.csv",data.root.path))      ## read in precipitation data 
MODIS <- read.csv(sprintf("%sMODIS.csv",data.root.path))    ## read in MODIS data 
precip.f <-read.csv(sprintf("%srainforecast.csv",data.root.path)) 

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


Date.Start = min(as.numeric(as.Date(GPM[,1])),as.numeric(as.Date(MODIS[,1])),as.numeric(as.Date(SMAP[,1])),as.numeric(as.Date(precip.f[,1],format='%m-%d-%Y')))
Date.End = max(as.numeric(as.Date(GPM[,1])),as.numeric(as.Date(MODIS[,1])),as.numeric(as.Date(SMAP[,1])),as.numeric(as.Date(precip.f[,1],format='%m-%d-%Y')))
combined = data.frame(Date=as.Date(character()),
                      NDVI=numeric(), 
                      Precip=numeric(),  
                      SoilMoisture=numeric(),
                      rainforecast=numeric(),
                      rainprob=numeric()) 
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
    precip=NA} ## note that change precip to NA if necessary
  
  smap = SMAP[as.Date(SMAP$Date)==i_this_day,2]
  if (is.null(smap)|length(smap)==0) {
    smap=NA}
  
  rain.f = precip.f[as.Date(precip.f[,1],format='%m-%d-%Y')==i_this_day,2]
  if (is.null(rain.f )|length(rain.f)==0 ){
    rain.f=NA}
  
  rain.prob = precip.f[as.Date(precip.f[,1],format='%m-%d-%Y')==i_this_day,3]
  if (is.null(rain.prob )|length(rain.prob)==0 ){
    rain.prob=NA}
  
  
  combined[n,2]=ndvi
  combined[n,3]=precip/10
  combined[n,4]=smap
  combined[n,5]=rain.f
  combined[n,6]=rain.prob
}
NonNAindex <- min(which(!is.na(combined$SoilMoisture)))-1
combined=combined[-(1:NonNAindex),]
row.names(combined) <- 1:nrow(combined)

write.table(combined,file = paste(data.root.path,'combined_data.csv',sep=""),na="NA",row.names=FALSE,col.names=TRUE,sep=",")