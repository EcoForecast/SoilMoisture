require(compiler)
source("SSSM.R")
source("00_tool_function.R")
source("00_global_variables.R")
SSSMbyte= cmpfun(SSSM)

### Initial State 
load('./jags.out.file.RData')
data.root.path = './example/'
combined <- as.data.frame(read.csv(sprintf("%scombined_data.csv",data.root.path)))
training_date_end = as.Date(training_date_end) # last training date

prediction_date = as.character(training_date_end +1) #first day to predict
prediction_date_idx = which(combined[,1]==prediction_date) # find the index
obs = combined[prediction_date_idx:nrow(combined),]
rm(combined)


#### SET THE ENSEMBLE SIZE
#n_ensemble = 5000 ## production run should be 200 - 5000, depending on what your computer can handle
#n_forecast = 14 # number of days to forecast
today = Sys.Date()
start_en_idx = which(obs[,1]==prediction_date)
today_en_idx=which(obs[,1]==as.character(today))
nt = today_en_idx-start_en_idx+1+n_forecast # e.g. forecast nrow(obs)+14
time_t = as.Date(obs$Date[start_en_idx:today_en_idx]) # time till today
time_f =  c(time_t,time_t[length(time_t)]+1:n_forecast) # time include the forecast
time_last_date2plot=as.Date(as.Date(prediction_date):last_date2plot,origin="1970-01-01")

## check the observed data
sm.obs = obs[start_en_idx:today_en_idx,"SoilMoisture"]
par(mfrow=c(1,1))
plot(time_t,sm.obs,ylab="SoilMoisture",lwd=2,main='Daily SoilMoisture (obs)',ylim=c(0,1))

#### Initialize inputs - last day value from jags models
mcmc.sm = exp(out[,grep("x[",colnames(out),fixed=T)]) # it is actually log(sm)
length=dim(mcmc.sm)[2]
mcmc.sm = mcmc.sm[,length] #last day soil moisture in jags model
mcmc.precip=out[,grep("p[",colnames(out),fixed=T)][,length] #last day precip in jags model
mcmc.ndvi = out[,grep("n[",colnames(out),fixed=T)][,length] #last day ndvi in jags model
X = as.matrix(c(median(mcmc.sm),median(mcmc.precip),median(mcmc.ndvi)))
initial.inputs=X # soilmoisture, precip, ndvi
if(n_ensemble > 1){
  X = as.matrix(cbind(
    rlnorm(n_ensemble,log(X[1]),sd(mcmc.sm)),
    rlnorm(n_ensemble,log(X[2]),sd(mcmc.precip)),
    rnorm(n_ensemble,X[3],sd(mcmc.ndvi))
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
params$beta_0  =  rlnorm(n_ensemble,log(median(mcmc.beta_0)),sd(mcmc.beta_0))
params$beta_1  =  rlnorm(n_ensemble,log(median(mcmc.beta_1)),sd(mcmc.beta_1))
params$beta_2  =  rnorm(n_ensemble,median(mcmc.beta_2),sd(mcmc.beta_2))

## Process and observation error
#params$tau_add = 1/sqrt(rgamma(n_ensemble,10,.04)) ## prior process error for model
#params$tau_obs = 1/sqrt(rgamma(n_ensemble,10,.06)) ## prior data error for soilmoisture
#params$tau_nobs = 1/sqrt(rgamma(n_ensemble,10,.05)) ## prior data error in ndvi

params$tau_add = 1/sqrt(rgamma(n_ensemble,10,1/var(mcmc.tau_add)*100)) ## prior process error for model
params$tau_obs = 1/sqrt(rgamma(n_ensemble,10,1/var(mcmc.tau_obs)*100)) ## prior data error for soilmoisture
params$tau_nobs = 1/sqrt(rgamma(n_ensemble,10,1/var(mcmc.tau_nobs)*100)) ## prior data error in ndvi

## Other priors
params$p.rate = rpois(n_ensemble,median(mcmc.p.rate))
params$p.rain = rnorm(n_ensemble,median(mcmc.p.rain),sd(mcmc.p.rain))


### Run SSSM
X = X.orig
output = array(0.0,c(nt,n_ensemble,3))
for(t in 1:nt){
  rain.f=obs[start_en_idx+nt-1,5:6]
  output[t,,]=SSSM(X,params,rain.f)
  X=output[t,,1:3]
  print(t) ## day counter
}

## Basic time-series visualizations for all SSSM model output variables
output[is.nan(output)] = 0
output[is.infinite(output)] = 0
varnames <- c("SoilMoisture","Precip","NDVI")
units <- c("cm3/cm3","mm","unitless")
par(mfrow=c(1,1))
for(i in 1:3){
  ci = apply(output[,,i],1,quantile,c(0.025,0.5,0.975))
  plot(time_f,ci[2,],main=varnames[i],xlab="time",ylab=units[i],type='l',ylim=range(ci))
  ciEnvelope(time_f,ci[1,],ci[3,],col=col.alpha("lightGrey",0.5))
  lines(ci[2,])
}

##Then, let's only focus on soilmoisture
sm.model.ci  = apply(output[,,1],1,quantile,c(0.025,0.5,0.975))
plot(time_f,sm.model.ci[2,],main=varnames[1],xlab="time",ylab=units[1],type='l',ylim=range(sm.model.ci))
ciEnvelope(time_f,sm.model.ci[1,],sm.model.ci[3,],col=col.alpha("lightGrey",0.5))
lines(sm.model.ci[2,])
points(time_t, sm.obs,col="red")

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
  rain.f=obs[start_en_idx+nt-1,5:6]
  output[t,,]=SSSM(X,params,rain.f)
  X=output[t,,1:3]
  ## analysis step
    sample = sample+1
    print(sample)
    if(!is.na(sm.obs[sample])){  ## if observation is present
      Lm = output[t, ,1] ## model soilmoisture over obs period
      wt = dnorm(sm.obs[sample],Lm,1/sqrt(median(mcmc.tau_obs)))
      ## resample 
      index = sample.int(n_ensemble,n_ensemble,replace=TRUE,prob=wt)
      X = X[index,]
      params = update.params(params,index)    
    }
    hist.params[[sample+1]] = params
}
save(output,output.ensemble,hist.params,initial.inputs,file="PF.output.RData") 


## Extract and summarize soilmoisture (pr = PF, resampling)
sm.pr.ci  = apply(output[,,1],1,quantile,c(0.025,0.5,0.975))
png(filename = sprintf("./example/gif/Forecast_PF_plot_%s.png",format(today, format="%Y%m%d")),
    width = 960, height = 480, units = "px", pointsize = 12,
    bg = "white",  res = NA)
plot(time_last_date2plot,rep(0.2,length(time_last_date2plot)),ylim=range(c(0,1)),
     type='n',ylab="Soil Moisture",xlab="time",main="Soil Moisture Forecast(resampling PF)",
     cex.lab=1.3, cex.axis=1.3, cex.main=1.3, cex.sub=1.3)
#plot(time_f,sm.model.ci[2,],ylim=range(c(range(sm.model.ci),range(sm.obs,na.rm=TRUE))),
#     type='n',ylab="Soil Moisture",xlab="time")
ciEnvelope(time_f,sm.model.ci[1,],sm.model.ci[3,],col=col.alpha("lightGrey",0.5))
ciEnvelope(time_f,sm.pr.ci[1,],sm.pr.ci[3,],col=col.alpha("lightGreen",0.5))
points(time_t, sm.obs,col="red")  
lines(time_f,sm.model.ci[2,],col="black")
lines(time_f,sm.pr.ci[2,],col="blue")
legend("topleft",lty=c(1,1,1,1,0),
       c('95% CI of Ensemble','95% CI of PF','Ensemble median','PF median','obs. sm'),
       col=c('lightGrey','lightGreen','black','blue','red'),
       pch=c(15,15,NA,NA,1),lwd=2,cex=1.15)
dev.off()


### assess shifts in any parameter values
png(filename = sprintf("./example/gif/Forecast_PF_params_plot_%s.png",format(today, format="%Y%m%d")),
    width = 480, height = 480, units = "px", pointsize = 12,
    bg = "white",  res = NA)
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
dev.off()
par(mfrow=c(1,1))














# daily.mean<-array(0,nt)
# range.ensemble<-array(0,n_ensemble)
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
# plot(combined[2,],type='l',ylim=range(combined),xlab='Days Since Start of time Series',main='Soil Moisture (cm^3/cm^3)', ylab='Soil Moisture')
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
