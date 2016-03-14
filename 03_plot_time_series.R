#Taking the three separate csv files for each data 
#source and plotting them as a time series

#Pseudocode
  ##define function for plotting the time series
    ##input each csv into function separately


#using defined timeseries function
timeseries.plot<-function(csv) {
  timeseries.dataframe<-read.csv(csv)    #read the csv file into a dataframe
  value.dataframe<-timeseries.dataframe[,2]
  timeseries<-ts(value.dataframe)
  plot.ts(timeseries,ylab='Value',main=csv)
  
}

#run function for all CSV files #update if needed for more MODIS
timeseries.SMAP<-timeseries.plot('SMAP.csv')
timeseries.GPM<-timeseries.plot('GPM.csv')
timeseries.MODIS<-timeseries.plot('MODIS.csv')

