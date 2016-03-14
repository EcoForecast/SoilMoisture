#Taking the three separate csv files for each data 
#source and plotting them as a time series

#Pseudocode
  ##define function for plotting the time series
    ##input each csv into function separately


#using defined timeseries function

library(tools)

timeseries.plot<-function(csv) {
  timeseries.dataframe<-read.csv(csv)    #read the csv file into a dataframe
  value.dataframe<-timeseries.dataframe[,2]
  timeseries<-ts(value.dataframe)
  name<-file_path_sans_ext(csv)
  mypath<-file.path('web/plots/',paste(name,'.png',sep=''))
  png(filename=mypath)
  plot.ts(timeseries,ylab='Value',main=csv)
  dev.off()
}

timeseries.plot('SMAP.csv')
timeseries.plot('MODIS.csv')
timeseries.plot('GPM.csv')


