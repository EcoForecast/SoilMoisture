require(compiler)
source("SSSM.R")
SSSMbyte= cmpfun(SSSM)
#### SET THE ENSEMBLE SIZE

ne = 10 ## production run should be 200 - 5000, depending on what your computer can handle
nt = 100 # forecast next 100 days

### Initial State 
load('./jags.out.file.RData')

mcmc.sm = exp(out[,grep("x",colnames(out))]) # it is actually log(sm)
mcmc.precip=out[,grep("p",colnames(out))]
mcmc.ndvi = exp(out[,grep("n",colnames(out))])
mcmc.beta_0 = out[,grep("beta_0",colnames(out))] #sm coef
mcmc.beta_1 = out[,grep("beta_1",colnames(out))] #precip coef
mcmc.beta_2 = out[,grep("beta_2",colnames(out))] #ndvi coef
mcmc.ncol = ncol(mcmc.sm) #  Number of cols is the number of days

X = as.matrix(c(median(mcmc.sm[,mcmc.ncol]),median(mcmc.precip[,mcmc.ncol]),median(mcmc.ndvi[,mcmc.ncol])))
if(ne > 1){
  X = as.matrix(cbind(
    rlnorm(ne,log(X[1]),sd(mcmc.sm[,mcmc.ncol])),
    rnorm(ne,X[2],sd(mcmc.precip[,mcmc.ncol ])),
    rlnorm(ne,log(X[3]),sd(mcmc.ndvi[,mcmc.ncol]))))
}
X.orig = X



### Define the priors on the model parameters
params = list()
params$beta_0  =  rlnorm(ne,log(median(mcmc.beta_0)),sd(mcmc.beta_0))
params$beta_1  =  rlnorm(ne,log(median(mcmc.beta_1)),sd(mcmc.beta_1))
params$beta_2  =  rlnorm(ne,log(median(mcmc.beta_2)),sd(mcmc.beta_2))

## Process error
params$tau.obs = 1/sqrt(rgamma(ne,0.01,0.01)) ## prior process error in soil moisture
params$tau.nobs = 1/sqrt(rgamma(ne,0.01,0.01)) ## prior process error in ndvi

### produce our initial ensemble forecast for the system
X = X.orig

output = array(0.0,c(nt,ne,3))
for(t in 1:nt){
  output[t,,]=SSSM(X,params)
  X=output[t,,1:3]
  print(t) ## day counter
}
