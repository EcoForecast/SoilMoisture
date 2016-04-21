# We aim to fuse Times-Series Data in a State Space Model: SMAP, GMP and MODIS

# In this test, soilmoisture is a SoilMoistureModel model

###################
#------------------ sub-routines,set your JAGS model here
predict.JAGS <- function(time,y,p,n) {
  require(rjags)
  require(coda)
  
  SoilMoistureModel = "
  model{
  
  #### Data Model
  for(t in 1:nt){
  y[t] ~ dnorm(x[t],tau_obs)
  }
  
  # Data model for NDVI 
  for(t in 2:nt){
#  n[t]~dnorm(n[t-1],tau_nobs)
   n[t]~dnorm(mu.ndvi,tau_nobs)
  }
  
  ## Data model for precip
  #for(t in 1:length(NA.indices)){
  #p[NA.indices[t]] ~ dlnorm(5, 1)
  #}
  for(t in 1:nt){
    rained[t] ~ dbern(p.rain)
    pr[t] <- p.rate*rained[t]+0.00001
    p[t] ~ dpois(pr[t])
  }
  
  #### Process Model
  for(t in 2:nt){
  SoilMoisture[t] <- beta_0*x[t-1] + beta_1*p[t-1] + beta_2*n[t-1]
  #Term 1: runoff
  #Term 2: Added impact from yesterday's rainfall (assuming 1 day delay)
  #Term 3: Effect of NDVI
  x[t]~dnorm(SoilMoisture[t],tau_add)
  }
  
  
  #### Priors
  p.rate ~ dunif(0,1000)
  p.rain ~ dbeta(1,1)
  mu.ndvi ~ dunif(-1,1)
  tau_obs ~ dgamma(a_obs,r_obs)
  tau_add ~ dgamma(a_add,r_add)
  beta_0 ~ dbeta(a_beta0,r_beta0)
  beta_1 ~ dgamma(a_beta1,r_beta1)
  beta_2 ~ dbeta(a_beta2,r_beta2)
  mu_p ~ dnorm(mu_p0, tau_p0)
  tau_p ~ dgamma(.01, .01)
  tau_nobs ~ dgamma(0.01,0.01)
  ## initial condition
  x[1] ~ dunif(x_ic_lower,x_ic_upper)  
  n[1] ~ dunif(0,1)  
  }
  "
  
  #data <- list(y=log(y),p=p, n=n, NA.indices=NA.indices, nt=length(y),x_ic_lower=log(0.000001),x_ic_upper=log(1), a_obs=0.01,
  data <- list(y=log(y),p=p, n=n, nt=length(y),x_ic_lower=log(0.000001),x_ic_upper=log(1), a_obs=0.01,
               r_obs=0.01,a_add=0.01, r_add=.01, a_beta0=2,r_beta0=2, a_beta1=2, r_beta1=2,
               a_beta2=1,r_beta2=2, mu_p0=3, tau_p0=3)
  
  
  nchain = 3
  init <- list()
  for(i in 1:nchain){
    y.samp = sample(y,length(y),replace=TRUE)
    init[[i]] <- list(tau_add=1/var(diff((log(y.samp)))),tau_obs=1/var((log(y.samp))))
  }
  
  j.model   <- jags.model (file = textConnection(SoilMoistureModel),
                           data = data,
                           inits = init,
                           n.chains = 3)
  
  ## burn-in
  jags.out   <- coda.samples (model = j.model,
                              variable.names = c("tau_add","tau_obs","beta_0","beta_1","beta_2"),
                              n.iter = 1000)
  # Only to plot 1000 iterations.  
  
  #plot(jags.out) 
  
  
  jags.out   <- coda.samples (model = j.model,
                              variable.names = c("x","p","n","tau_add","tau_obs","tau_nobs","beta_0","beta_1","beta_2"),
                              n.iter = 10000)
  
  
  #summary of the final 10000 iteration jags.out
  #summary(jags.out)
  return(jags.out)
  
}

#-------------plots a confidence interval around an x-y plot (e.g. a timeseries)
ciEnvelope <- function(x,ylo,yhi,...){
  polygon(cbind(c(x, rev(x), x[1]), c(ylo, rev(yhi),ylo[1])), border = NA,...) 
}


#-------------load data from combined csv
## set working directory 
data.root.path = './example/'
# Soil Moisture (cm^3 of water per cm^3 of soil)
combined <- as.data.frame(read.csv(sprintf("%scombined_data.csv",data.root.path)))
#combined<-combined[0:50,]
#remove NA values
# require(zoo)
# #interpolate between values keeping NA
# combined$NDVI<-na.approx(combined$NDVI,na.rm=FALSE)    #reset
##apply first valid value to NA values at beginning
#idx.not.na=which(!is.na(combined$NDVI))
#combined$NDVI[is.na(combined$NDVI)]<-combined$NDVI[idx.not.na[1]]


#-------------Run JAGS, and Do some plots
time = as.Date(combined$Date)
y = combined$SoilMoisture
p = combined$Precip
n = combined$NDVI

nf = 40
time = as.Date(combined$Date) 
time = c(time,time[length(time)]+1:nf)
y = c(combined$SoilMoisture,rep(NA,nf))
p = c(combined$Precip,rep(NA,nf))
n = c(combined$NDVI,rep(NA,nf))

# plot original weekly observation data
plot(time,y,ylab="SoilMoisture",lwd=2,main='Daily SoilMoisture', ylim=c(0,.6))

jags.out.original = predict.JAGS(time,y,p,n)
out <- as.matrix(jags.out.original)


# plot the original result (weekly observation frequency)
par(mfrow=c(1,1))
time.rng = c(1,length(time)) ## adjust to zoom in and out
ci <- apply(exp(out[,grep("x",colnames(out))]),2,quantile,c(0.025,0.5,0.975))

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

######
ci.ndvi <- apply(out[,grep("n[",colnames(out),fixed = TRUE)],2,quantile,c(0.025,0.5,0.975))
ci.precip <- apply((out[,grep("p",colnames(out))]),2,quantile,c(0.025,0.5,0.975))
lines(time,ci.precip[2,]/2000,col="blue")
lines(time,ci.ndvi[2,]/2,col="green")

# save output --------------------------
save(ci, out, file='jags.out.file.RData')


# Diagnostic plots ----------------------
## Standard Deviations for random effect taus
par(mfrow=c(2,3))
prec = out[,grep("tau",colnames(out), value=TRUE)]
for(i in 1:ncol(prec)){
  hist(1/sqrt(prec[,i]),main=colnames(prec)[i])
}
par(mfrow=c(1,1))






