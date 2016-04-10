library(compiler)

#Load data from state space model
file_name = 'jags.out.file'
load(file_name)

#Get average soil moisture over the last week for initial condition
last.week.sm <- mean()

working_df <- as.data.frame(out)
colnames(working_df)

#Get average soil moisture over the last week for initial condition
number.observations <- dim(ci)[2]
number.ensembles <- 50 #how to define this? 
time.to.forecast <- 7 #forecasting 7 days into the future

last.week.sm <- mean(ci[2,(number.observations-7):number.observations])
ensemble.forecasts = matrix(NA,time.to.forecast, number.ensembles)
ensemble.forecasts[1,] <- last.week.sm
