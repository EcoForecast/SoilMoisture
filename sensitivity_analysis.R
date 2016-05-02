# Sensitivity Analysis 
########################

load("PF.output.RData")

pdf("Initial_Forecast.pdf")
plot.variables <- function(reg.x,label){
  reg.y=output[2,,1]
  reg <- lm(reg.y~reg.x,model=TRUE)
  plot(reg.x,reg.y,xlab=label,ylab='Soil Moisture',pch=20)
  abline(reg$coefficients, col='Red')
  r.sq <- summary(reg)$r.squared
  legend("topleft",c(paste("Sensitivity (Slope) =", format((reg$coefficients[2]), digits=3)),paste("Uncertainty (R^2) =", format(r.sq,digits=3))),pt.cex=1,cex=0.7) 
}

plot.variables(params$beta_0,"Beta 0 Soil Moisture Persistance")
plot.variables(params$beta_1,"Beta 1 Influence of Rainfall")
plot.variables(params$beta_2,"Beta 2 Influence of NDVI")
plot.variables(output[2,,2],"Rainfall (mm)")
plot.variables(output[2,,3],"NDVI")

dev.off()


# daily.mean<-array(0,nt)
# range.ensemble<-array(0,n_ensemble)
# for(i in 1:length(daily.mean)){
#  daily.mean[i]<-mean(output[i,,1])
# }

