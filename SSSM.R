# Super simple soilmoisture prediction model
# X = [soilmoisture[t-1], precip[t-1], ndvi[t-1] ]
SSSM<-function(X,params){
  ne = nrow(X)
  SoilMoisture = (X[,1])*params$beta_0*.8 +  X[,2]*params$beta_1 + (X[,3])*params$beta_2
  
  X1 = rnorm(ne,SoilMoisture,params$tau.obs) # SM
  X2 = rpois(ne,50)*rbinom(ne,1,.25) #distribution of rain given rain event based on binomial 0-1 chance distribution.
  X3 = rnorm(ne,X[,3],.005) #ndvi
  
  
  return(cbind(X1,X2,X3))
}

#temp<-array(0,nt)
#for(i in 1:length(temp)){
 # temp[i]<-mean(output[i,,1])
#}
