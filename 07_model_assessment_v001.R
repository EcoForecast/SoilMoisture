# Model assessment 
##########################

# Use a series of visualizations and statistical measures 
# to assess the performance of Super Simple Soil Moisture Model with SMAP soil moisture data 

## load libraries
library("plotrix")
library(dplR)
library(rpart)
library(randomForest)

# load PF output 
load("PF.output.RData")
source("00_global_variables.R")
data.root.path = './example/'
combined <- as.data.frame(read.csv(sprintf("%scombined_data.csv",data.root.path)))
prediction_date_idx = which(combined[,1]==prediction_date) # find the index

# soil moisture observation error
O = combined[prediction_date_idx:nrow(combined),4]
time_p <- as.Date(combined[prediction_date_idx:nrow(combined),1])

# mean of soil moisture (the 1st element), 1 indicates rows; for both simple sm model and for 
# particle filter
sm.ens = apply(output.ensemble[,,1],1,mean)[1:length(prediction_date_idx:nrow(combined))]
sm.pf  = apply(output[,,1],1,mean)[1:length(prediction_date_idx:nrow(combined))]



## Model vs obs regressions
sm.ens.fit = lm(O ~ sm.ens)
sm.pf.fit = lm(O ~ sm.pf)

## performance stats
stats = as.data.frame(matrix(NA,4,2))
rownames(stats) <- c("RMSE","Bias","cor","slope")
colnames(stats) <- c("ens","pf")
stats["RMSE",'ens'] = sqrt(mean((sm.ens-O)^2, na.rm=TRUE))
stats["RMSE",'pf']  = sqrt(mean((sm.pf-O)^2, na.rm=TRUE))
stats['Bias','ens'] = mean(sm.ens-O, na.rm=TRUE)
stats['Bias','pf']  = mean(sm.pf-O, na.rm=TRUE)
stats['cor','ens']  = cor(sm.ens,O, use="complete.obs")
stats['cor','pf']   = cor(sm.pf,O, use="complete.obs")
stats['slope','ens'] = coef(sm.ens.fit)[2]
stats['slope','pf']  = coef(sm.pf.fit)[2]
knitr::kable(stats)

## predicted-observed
plot(sm.ens,O,pch=20,xlab="ensemble",ylab='observed', xlim=c(0.15,0.5), ylim=c(0.15,0.5), main='Soil Moisture')
abline(0,1,col=2,lwd=2)
abline(sm.ens.fit,col=3,lwd=3,lty=2)
legend("topleft",legend=c('obs','1:1','reg'),col=1:3,lwd=3)

plot(sm.pf,O,pch=20,xlab="particle filter",ylab='observed', xlim=c(0.15,0.5), ylim=c(0.15,0.5),main='Soil Moisture')
abline(0,1,col=2,lwd=2)
abline(sm.pf.fit,col=3,lwd=3,lty=2)
legend("topleft",legend=c('obs','1:1','reg'),col=1:3,lwd=3)


## example cycle
plot(time_p,O,lwd=2,ylim=c(0,1),xlab="Day of Year",ylab="Soil Moisture", pch=20)
points(time_p,sm.ens,col=4,lwd=2,lty=2, pch=20)
points(time_p,sm.pf,col=2,lwd=2,lty=2, pch=20)
legend("topright",legend=c("Obs", "ens", "pf"),col=c(1,4,2,3), pch=20)

