#setwd('/Users/ericbullock/Google Drive/Class/Ecological_Forecasting/Project/SoilMoisture')
require(compiler)
source("SSSM.R")
source("00_tool_function.R")
SSSMbyte= cmpfun(SSSM)
### Initial State 
load('./jags.out.file.RData')
data.root.path = './example/'
combined <- as.data.frame(read.csv(sprintf("%scombined_data.csv",data.root.path)))
training_date = as.Date('2016-02-29') # last training date
prediction_date = as.character(training_date +1) #first day to predict
prediction_date_idx = which(combined[,1]==prediction_date) # find the index
obs = combined[prediction_date_idx:nrow(combined),]
rm(combined)
sm.obs = obs[,"SoilMoisture"]
time=as.Date(obs$Date)
plot(time,sm.obs,ylab="SoilMoisture",lwd=2,main='Daily SoilMoisture (obs)',ylim=c(0,1))

#### SET THE ENSEMBLE SIZE
ne = 500 ## production run should be 200 - 5000, depending on what your computer can handle
nt = nrow(obs) # e.g. forecast next 100 days

#### Initialize inputs - last day value from jags models
mcmc.sm = exp(out[,grep("x[",colnames(out),fixed=T)]) # it is actually log(sm)
length=dim(mcmc.sm)[2]
mcmc.sm = mcmc.sm[,length] #last day soil moisture in jags model
mcmc.precip=out[,grep("p[",colnames(out),fixed=T)][,length] #last day precip in jags model
mcmc.ndvi = out[,grep("n[",colnames(out),fixed=T)][,length] #last day ndvi in jags model
X = as.matrix(c(median(mcmc.sm),median(mcmc.precip),median(mcmc.ndvi)))
initial.inputs=X # soilmoisture, precip, ndvi
if(ne > 1){
  X = as.matrix(cbind(
    rlnorm(ne,log(X[1]),sd(mcmc.sm)),
    rlnorm(ne,log(X[2]),sd(mcmc.precip)),
    rnorm(ne,X[3],sd(mcmc.ndvi))
  ))
}
X.orig = X
####

####parameters extract from JAGS model
mcmc.beta_0 = out[,grep("beta_0",colnames(out))] #sm coef
mcmc.beta_1 = out[,grep("beta_1",colnames(out))] #precip coef
mcmc.beta_2 = out[,grep("beta_2",colnames(out))] #ndvi coef
mcmc.tau_add = out[,grep("tau_add",colnames(out))] #tau_add, process error for model
mcmc.tau_nobs = out[,grep("tau_nobs",colnames(out))] #observation error for ndvi
mcmc.tau_obs = out[,grep("tau_obs",colnames(out))] #observation error for soilmoisture
mcmc.p.rate = out[,grep("p.rate",colnames(out))]
mcmc.p.rain = out[,grep("p.rain",colnames(out))]
mcmc.mu.ndvi=out[,grep("mu.ndvi",colnames(out))]
#####


### Define the priors for the SSSM model parameters
params = list()
params$beta_0  =  rlnorm(ne,log(median(mcmc.beta_0)),sd(mcmc.beta_0))
params$beta_1  =  rlnorm(ne,log(median(mcmc.beta_1)),sd(mcmc.beta_1))
params$beta_2  =  rnorm(ne,median(mcmc.beta_2),sd(mcmc.beta_2))

## Process and observation error
params$tau_add = 1/sqrt(rgamma(ne,10,.04)) ## prior process error for model
params$tau_obs = 1/sqrt(rgamma(ne,10,.06)) ## prior data error for soilmoisture
params$tau_nobs = 1/sqrt(rgamma(ne,10,.05)) ## prior data error in ndvi
## Other priors
params$p.rate = rpois(ne,median(mcmc.p.rate))
params$p.rain = rnorm(ne,median(mcmc.p.rain),sd(mcmc.p.rain))


### Run SSSM
X = X.orig
output = array(0.0,c(nt,ne,3))
for(t in 1:nt){
  output[t,,]=SSSM(X,params)
  X=output[t,,1:3]
  print(t) ## day counter
}

## Basic time-series visualizations for all SSSM model output variables
output[is.nan(output)] = 0
output[is.infinite(output)] = 0
varnames <- c("SoilMoisture","Precip","NDVI")
units <- c("cm3/cm3","mm","unitless")
for(i in 1:3){
  ci = apply(output[,,i],1,quantile,c(0.025,0.5,0.975))
  plot(time,ci[2,],main=varnames[i],xlab="time",ylab=units[i],type='l',ylim=range(ci))
  ciEnvelope(time,ci[1,],ci[3,],col=col.alpha("lightGrey",0.5))
  lines(ci[2,])
}

##Then, let only focus on soilmoisture
sm.model.ci  = apply(output[,,1],1,quantile,c(0.025,0.5,0.975))
plot(time,sm.model.ci[2,],main=varnames[1],xlab="time",ylab=units[1],type='l',ylim=range(sm.model.ci))
ciEnvelope(time,sm.model.ci[1,],sm.model.ci[3,],col=col.alpha("lightGrey",0.5))
lines(sm.model.ci[2,])
points(time, sm.obs,col="red")



#### Then do resampling particle filter
update.params <- function(params,index){
  params$beta_0  = params$beta_0[index]
  params$beta_1    = params$beta_1[index]
  params$beta_2  = params$beta_2[index]
  params$tau_add     =  params$tau_add[index]
  params$tau_obs  = params$tau_obs[index]
  params$tau_nobs  = params$tau_nobs[index]
  params$p.rain     = params$p.rain [index]
  params$p.rate  = params$p.rate[index]
  
  return(params)
}
## resampling particle filter
sample=0
hist.params=list()  ## since we resample parameters, create a record (history) of what values were used each step
hist.params[[1]] = params
X = X.orig  ## reset state to the initial values, not the final values from the previous ensemble
output.ensemble = output ## save original projection
for(t in 1:nt){
  ## forward step
  output[t,,]=SSSM(X,params)
  X=output[t,,1:3]
  ## analysis step
    sample = sample+1
    print(sample)
    if(!is.na(sm.obs[sample])){  ## if observation is present
      ## calulate Likelihood (weights)
      Lm = output[t, ,1] ## model soilmoisture over obs period
      wt = dnorm(sm.obs[sample],Lm,1/sqrt(median(mcmc.tau_obs)))
      ## resample 
      index = sample.int(ne,ne,replace=TRUE,prob=wt)
      X = X[index,]
      params = update.params(params,index)    
    }
    hist.params[[sample+1]] = params
}
save(output,output.ensemble,hist.params,initial.inputs,file="PF.output.RData") 


## Extract and summarize soilmoisture (pr = PF, resampling)
sm.pr.ci  = apply(output[,,1],1,quantile,c(0.025,0.5,0.975))


plot(time,sm.model.ci[2,],ylim=range(c(range(sm.model.ci),range(sm.obs,na.rm=TRUE))),
     type='n',ylab="LAI",xlab="Time")

ciEnvelope(time,sm.model.ci[1,],sm.model.ci[3,],col=col.alpha("lightGrey",0.5))
ciEnvelope(time,sm.pr.ci[1,],sm.pr.ci[3,],col=col.alpha("lightGreen",0.5))
points(time, sm.obs,col="red")  
lines(time,sm.model.ci[2,],col="black")
lines(time,sm.pr.ci[2,],col="blue")
legend("topleft",lty=c(1,1,1,1,0),
       c('95% CI of Ensemble','95% CI of PF','Ensemble median','PF median','obs. sm'),
       col=c('lightGrey','lightGreen','black','blue','red'),
       pch=c(15,15,NA,NA,1),lwd=2,cex=0.65)



### assess shifts in any parameter values
par(mfrow=c(3,3))
par(mar=c(2,2,4,0.7))
for(i in 1:length(params)){
  if(is.null(dim(params[[i]]))){ ## parameter is scalar
    orig = density(hist.params[[1]][[i]])
    new = density(params[[i]])
    ylim=range(c(range(new$y),range(orig$y)))
    plot(orig,main=names(params)[i],xlab=" ",
         ylim=ylim)
    lines(new,col=2,lwd=2)
    text(max(orig$x),ylim[2]*0.95,
         paste(format(mean(hist.params[[1]][[i]]),digits=3),
               format(sd(hist.params[[1]][[i]]),digits=3)),
         pos=2)
    text(max(orig$x),ylim[2]*0.85,
         paste(format(mean(params[[i]]),digits=3),
               format(sd(params[[i]]),digits=3)),
         pos=2,col=2)
  } else {
    ## parameter is vector
    for(j in 1:ncol(params[[i]])){
      orig = density(hist.params[[1]][[i]][,j])
      new = density(params[[i]][,j])
      ylim=range(c(range(new$y),range(orig$y)))
      plot(orig,main=paste(names(params)[i],j), xlab=" ",
           ylim=ylim)
      lines(new,col=2,lwd=2)
      text(max(orig$x),ylim[2]*0.95,
           paste(format(mean(hist.params[[1]][[i]][,j]),digits=3),
                 format(sd(hist.params[[1]][[i]][,j]),digits=3)),
           pos=2)
      text(max(orig$x),ylim[2]*0.85,
           paste(format(mean(params[[i]][,j]),digits=3),
                 format(sd(params[[i]][,j]),digits=3)),
           pos=2,col=2)
    }      
  }  
}
par(mfrow=c(1,1))














# daily.mean<-array(0,nt)
# range.ensemble<-array(0,ne)
# for(i in 1:length(temp)){
#  daily.mean[i]<-mean(output[i,,1])
# }
# 
# 
# 
# pdf("Initial_Forecast.pdf")
# plot.variables <- function(reg.x,label){
#   reg.y=output[2,,1]
#   reg <- lm(reg.y~reg.x,model=TRUE)
#   plot(reg.x,reg.y,xlab=label,ylab='Soil Moisture',pch=20)
#   abline(reg$coefficients, col='Red')
#   r.sq <- summary(reg)$r.squared
#   legend("topleft",c(paste("Sensitivity (Slope) =", format((reg$coefficients[2]), digits=3)),paste("Uncertainty (R^2) =", format(r.sq,digits=3))),pt.cex=1,cex=0.7) 
# }
# 
# 
# plot(combined[2,],type='l',ylim=range(combined),xlab='Days Since Start of Time Series',main='Soil Moisture (cm^3/cm^3)', ylab='Soil Moisture')
# ciEnvelope(1:ncol(combined),combined[1,],combined[3,],col=col.alpha("lightGrey",0.5))
# abline(v=length)
# legend("topleft","Line Represents Start of Particle Filter Forecast",pt.cex=1,cex=0.7)
# 
# plot.variables(params$beta_0,"Beta 0 Soil Moisture Persistance")
# plot.variables(params$beta_1,"Beta 1 Influence of Rainfall")
# plot.variables(params$beta_2,"Beta 2 Influence of NDVI")
# plot.variables(output[2,,2],"Rainfall (mm)")
# plot.variables(output[2,,3],"NDVI")
# 
# dev.off()
# 
# 
# 
# 
# 
# 
# 
