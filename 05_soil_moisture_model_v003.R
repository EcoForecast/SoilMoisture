# We aim to fuse Times-Series Data in a State Space Model: SMAP, GMP and MODIS
source("00_tool_function.R")
source("soilmoisture_JAGS.R")

#-------------load data from combined csv
## set working directory 
data.root.path = './example/'
# Soil Moisture (cm^3 of water per cm^3 of soil)
combined <- as.data.frame(read.csv(sprintf("%scombined_data.csv",data.root.path)))
training_date = '2016-02-29'
training_date_idx = which(combined[,1]==training_date)
data2training = combined[1:training_date_idx,]

#-------------Run JAGS, and Do some plots
time = as.Date(data2training$Date)
y = data2training$SoilMoisture
p = data2training$Precip*10 #scaled by 10, and do not forget to scale it back by 0.1 later
n = data2training$NDVI

### use JAGS to do pseudo-forescast, just for test use
# nf = 40
# time = as.Date(data2training$Date) 
# time = c(time,time[length(time)]+1:nf)
# y = c(data2training$SoilMoisture,rep(NA,nf))
# p = c(data2training$Precip,rep(NA,nf))
# n = c(data2training$NDVI,rep(NA,nf))
###

# plot original weekly observation data
plot(time,y,ylab="SoilMoisture",lwd=2,main='Daily SoilMoisture', ylim=c(0,.6))

# call the JAGS model
jags.out.original = predict.JAGS(time,y,p,n)
out <- as.matrix(jags.out.original)

# plot the original result (weekly observation frequency)
par(mfrow=c(1,1))
time.rng = c(1,length(time)) ## adjust to zoom in and out
ci <- apply(exp(out[,grep("x[",colnames(out),fixed = TRUE)]),2,quantile,c(0.025,0.5,0.975))
ylim = c(0,1)#range(ci,na.rm=TRUE); ylim[2] = min(ylim[2],1)
plot(time,ci[2,],type='n',ylim=ylim,ylab="Soil Moisture (cm^3/cm^3)",xlab='Date',xlim=time[time.rng], main='SoilMoistureModel')

## adjust x-axis label to be monthly if zoomed
# if(diff(time.rng) < 100){ 
#   axis.Date(1, at=seq(time[time.rng[1]],time[time.rng[2]],by='month'), format = "%Y-%m")
# }

ciEnvelope(time,(ci[1,]),(ci[3,]),col="lightBlue")
points(time,y,pch="+",cex=0.5)
#points(time[1:20],y[1:20],pch="o",col="red",cex=2)
points(time,p/2000,pch="o",col="blue",cex=1)
points(time,n/2,pch="o",col="green",cex=1)
lines(time,ci[2,],col="red")

###### add ndvi, precip on the plot to check if everything looks reasonable
ci.ndvi <- apply(out[,grep("n[",colnames(out),fixed = TRUE)],2,quantile,c(0.025,0.5,0.975))
ci.precip <- apply((out[,grep("p[",colnames(out),fixed = TRUE)]),2,quantile,c(0.025,0.5,0.975))
lines(time,ci.precip[2,]/2000,col="blue")
lines(time,ci.ndvi[2,]/2,col="green")

# save output --------------------------
save(ci, out, file='jags.out.file.RData')


# Diagnostic plots ----------------------
## Standard Deviations for random effect taus
#layout(matrix(c(1,2,3,3),2,2,byrow=TRUE))
par(mfrow=c(2,3))
prec = out[,grep("tau",colnames(out), value=TRUE)]
for(i in 1:ncol(prec)){
  hist(1/sqrt(prec[,i]),main=colnames(prec)[i])
}

prec1 = out[,grep("tau|beta_0|beta_1|beta_2",colnames(out), value=TRUE)]
cor(prec1)
#pairs(prec1)

par(mfrow=c (1,1))
plot(ci[2,], y,   xlab="Predicted", ylab="Observed", pch=20, xlim=range(y,na.rm=TRUE))
lmfit <- lm(y~ci[2,])
abline(lmfit, col="red")
abline(0,1, col="blue", lwd=1.5, lty=2)
summary(lmfit)
par(mfrow=c(1,1))






