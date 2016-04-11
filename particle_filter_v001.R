setwd('/Users/ericbullock/Google Drive/Class/Ecological_Forecasting/Project/SoilMoisture')
require(compiler)
source("SSSM.R")
SSSMbyte= cmpfun(SSSM)
#### SET THE ENSEMBLE SIZE

ne = 200 ## production run should be 200 - 5000, depending on what your computer can handle
nt = 100 # forecast next 100 days

### Initial State 
load('./jags.out.file.RData')

variables=list(params$beta_0, params$beta_1, params$beta_2)
mcmc.sm = exp(out[,grep("x",colnames(out))]) # it is actually log(sm)
length=dim(mcmc.sm)[2]
index=length-7
mcmc.precip=out[,grep("p",colnames(out))][,index:length]
mcmc.ndvi = exp(out[,grep("n",colnames(out))])[,index:length]
mcmc.beta_0 = out[,grep("beta_0",colnames(out))] #sm coef
mcmc.beta_1 = out[,grep("beta_1",colnames(out))] #precip coef
mcmc.beta_2 = out[,grep("beta_2",colnames(out))] #ndvi coef
mcmc.ncol = ncol(mcmc.sm[,index:length]) #  Number of cols is the number of days

X = as.matrix(c(median(mcmc.sm[,mcmc.ncol]),median(mcmc.precip[,mcmc.ncol]),median(mcmc.ndvi[,mcmc.ncol])))
if(ne > 1){
  X = as.matrix(cbind(
    rnorm(ne,(X[1]),sd(mcmc.sm[,mcmc.ncol])),
    rnorm(ne,X[2],sd(mcmc.precip[,mcmc.ncol ])),
    rnorm(ne,(X[3]),sd(mcmc.ndvi[,mcmc.ncol]))))
}
X.orig = X



### Define the priors on the model parameters
params = list()
#params$beta_0  =  rep(mean(mcmc.beta_0),ne)
#params$beta_1  =  rep(median(mcmc.beta_1),ne)
#params$beta_2  = rep(median(mcmc.beta_2),ne)

params$beta_0  =  rlnorm(ne,log(median(mcmc.beta_0)),sd(mcmc.beta_0))
params$beta_1  =  rlnorm(ne,log(median(mcmc.beta_1)),sd(mcmc.beta_1))
params$beta_2  =  rlnorm(ne,log(median(mcmc.beta_2)),sd(mcmc.beta_2))

## Process error
params$tau.obs = 1/sqrt(rgamma(ne,10,.05)) 
#params$tau.obs = rnorm(ne,0,.1) ## prior process error in soil moisture
params$tau.nobs = 1/sqrt(rbeta(ne,.01,.01)) ## prior process error in ndvi

### produce our initial ensemble forecast for the system
X = X.orig

output = array(0.0,c(nt,ne,3))
for(t in 1:nt){
  output[t,,]=SSSM(X,params)
  X=output[t,,1:3]
  print(t) ## day counter
}

daily.mean<-array(0,nt)
range.ensemble<-array(0,ne)
for(i in 1:length(temp)){
 daily.mean[i]<-mean(output[i,,1])
}



pdf("Initial_Forecast.pdf")
plot.variables <- function(reg.x,label){
  reg.y=output[2,,1]
  reg <- lm(reg.y~reg.x,model=TRUE)
  plot(reg.x,reg.y,xlab=label,ylab='Soil Moisture',pch=20)
  abline(reg$coefficients, col='Red')
  r.sq <- summary(reg)$r.squared
  legend("topleft",c(paste("Sensitivity (Slope) =", format((reg$coefficients[2]), digits=3)),paste("Uncertainty (R^2) =", format(r.sq,digits=3))),pt.cex=1,cex=0.7) 
}


plot(combined[2,],type='l',ylim=range(combined),xlab='Days Since Start of Time Series',main='Soil Moisture (cm^3/cm^3)', ylab='Soil Moisture')
ciEnvelope(1:ncol(combined),combined[1,],combined[3,],col=col.alpha("lightGrey",0.5))
abline(v=length)
legend("topleft","Line Represents Start of Particle Filter Forecast",pt.cex=1,cex=0.7)

plot.variables(params$beta_0,"Beta 0 Soil Moisture Persistance")
plot.variables(params$beta_1,"Beta 1 Influence of Rainfall")
plot.variables(params$beta_2,"Beta 2 Influence of NDVI")
plot.variables(output[2,,2],"Rainfall (mm)")
plot.variables(output[2,,3],"NDVI")

dev.off()




ciEnvelope <- function(x,ylo,yhi,...){
polygon(cbind(c(x, rev(x), x[1]), c(ylo, rev(yhi),
                                    ylo[1])), border = NA,...) 
}




