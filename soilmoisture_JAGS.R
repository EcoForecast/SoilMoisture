###################
#set your JAGS model here
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
  for(t in 1:nt){
  rained[t] ~ dbern(p.rain)
  pr[t] <- p.rate*rained[t]+0.00001
  p[t] ~ dpois(pr[t])
  }
  
  #### Process Model
  for(t in 2:nt){
  SoilMoisture[t] <- beta_0*x[t-1] + beta_1*p[t-1]/10 - beta_2*n[t-1] 
  
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
  beta_2 ~ dnorm(0.0,0.0001)
  #mu_p ~ dnorm(mu_p0, tau_p0)
  #tau_p ~ dgamma(.01, .01)
  tau_nobs ~ dgamma(0.01,0.01)
  ## initial condition
  x[1] ~ dunif(x_ic_lower,x_ic_upper)  
  n[1] ~ dunif(0,1)  
  }
  "
  
  data <- list(y=log(y),p=p, n=n, nt=length(y),x_ic_lower=log(0.000001),x_ic_upper=log(1), a_obs=0.01,
               r_obs=0.01,a_add=0.01, r_add=.01, a_beta0=2,r_beta0=2, a_beta1=2, r_beta1=2)
  
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
                              variable.names = c("tau_add","tau_obs","beta_0","beta_1","beta_2","p.rate","p.rain","mu.ndvi"),
                              n.iter = 1000)
  # Only to plot 1000 iterations.  
  
  #plot(jags.out) 
  
  
  jags.out   <- coda.samples (model = j.model,
                              variable.names = c("x","p","n","tau_add","tau_obs","tau_nobs","beta_0","beta_1","beta_2","p.rain","p.rate","mu.ndvi"),
                              n.iter = 10000)
  #summary(jags.out)
  return(jags.out)
  
}