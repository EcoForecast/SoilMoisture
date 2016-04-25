# Super simple soilmoisture prediction model
# X = [soilmoisture[t-1], precip[t-1], ndvi[t-1] ]
SSSM<-function(X,params){
  ne = nrow(X)
  SoilMoisture = log(X[,1])*params$beta_0 +  X[,2]*params$beta_1 - X[,3]*params$beta_2
  
  ## update states
  X1 = rnorm(ne,SoilMoisture,params$tau_add) # Soli Moisture
  X1 = pmin(pmax(exp(X1),0),1)
  
  rained=rbinom(ne, 1, params$p.rain)
  pr = rpois(ne,params$p.rate)
  X2 = rained*pr

  X3 = pmin(pmax(rnorm(ne,X[,3],params$tau_nobs),-1),1)
  return(cbind(X1,X2,X3))
}

