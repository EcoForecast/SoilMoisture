# We aim to fuse Times-Series Data in a State Space Model: SMAP, GMP and MODIS

# Comment by Chi . 03/25/2016
# In this test, soilmoisture is a random walk model

#------------- Loading libraries
require(rjags)
require(coda)

###################
#------------------ sub-routines,set your JAGS model here
predict.JAGS <- function(time,y) {
  require(rjags)
  require(coda)
  RandomWalk = "
  model{
  
  #### Data Model
  for(i in 1:n){
  y[i] ~ dnorm(x[i],tau_obs)
  }
  
  #### Process Model
  for(i in 2:n){
  x[i]~dnorm(x[i-1],tau_add)
  }
  
  #### Priors
  x[1] ~ dunif(x_ic_lower,x_ic_upper)
  tau_obs ~ dgamma(a_obs,r_obs)
  tau_add ~ dgamma(a_add,r_add)
  }
  "
  
  data <- list(y=y,n=length(y),x_ic_lower=0,x_ic_upper=1,a_obs=1,r_obs=1,a_add=1,r_add=1)
  
  nchain = 3
  init <- list()
  for(i in 1:nchain){
    y.samp = sample(y,length(y),replace=TRUE)
    init[[i]] <- list(tau_add=1/var(diff((y.samp))),tau_obs=5/var((y.samp)))
  }
  
  j.model   <- jags.model (file = textConnection(RandomWalk),
                           data = data,
                           inits = init,
                           n.chains = 3)
  
  ## burn-in
  jags.out   <- coda.samples (model = j.model,
                              variable.names = c("tau_add","tau_obs"),
                              n.iter = 1000)
  # Only to plot 1000 iterations.  
  #plot(jags.out) 
  
  jags.out   <- coda.samples (model = j.model,
                              variable.names = c("x","tau_add","tau_obs"),
                              n.iter = 10000)
  
  #summary of the final 10000 iteration jags.out
  #summary(jags.out)
  
}

#-------------plots a confidence interval around an x-y plot (e.g. a timeseries)
ciEnvelope <- function(x,ylo,yhi,...){
  polygon(cbind(c(x, rev(x), x[1]), c(ylo, rev(yhi),ylo[1])), border = NA,...) 
}


#-------------load data and merge datasets
#setwd("/Users/stanimirova/Desktop")  ## set working directory 
data.root.path = '/Users/chichen/Desktop/'
SMAP <- read.csv(sprintf("%sSMAP.csv",data.root.path))    ## read in soil moisture data 
GPM <- read.csv(sprintf("%sGPM.csv",data.root.path))      ## read in precipitation data 
MODIS <- read.csv(sprintf("%sMODIS.csv",data.root.path))    ## read in MODIS data 


#-------------Run JAGS, and Do some plots
time = as.Date(SMAP$Date)
y = SMAP$Data

# plot original weekly observation data
plot(time,y,type='l',ylab="SoilMoisture",lwd=2,main='Daily SoilMoisture')

jags.out.original = predict.JAGS(time,y)
par(mfrow=c(1,1))

# plot the original result (weekly observation frequency)
time.rng = c(1,length(time)) ## adjust to zoom in and out
out <- as.matrix(jags.out.original)
ci <- apply((out[,3:ncol(out)]),2,quantile,c(0.025,0.5,0.975))

plot(time,ci[2,],type='n',ylim=range(ci,na.rm=TRUE),ylab="SoilMoisture",xlim=time[time.rng], main='Random Walk')
## adjust x-axis label to be monthly if zoomed
if(diff(time.rng) < 100){ 
  axis.Date(1, at=seq(time[time.rng[1]],time[time.rng[2]],by='month'), format = "%Y-%m")
}
ciEnvelope(time,ci[1,],ci[3,],col="lightBlue")
points(time,y,pch="+",cex=0.5)

