# Super simple soilmoisture prediction model
# X = [soilmoisture[t-1], precip[t-1], ndvi[t-1] ]
SSSM<-function(X,params){
  ne = nrow(X)
  SoilMoisture = log(X[,1])*params$beta_0 +  X[,2]*params$beta_1 + log(X[,3])*params$beta_2
  
  X1 = rnorm(ne,SoilMoisture,params$tau.obs) # SM
  X2 = rpois(ne,50) #precip
  X3 = rnorm(ne,log(X[,3]),params$tau.obs) #ndvi
  
  
  return(cbind(X1,X2,X3))
}