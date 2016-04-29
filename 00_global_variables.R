## This script defines the global variables.
## please be very careful to set a global variable (do not mess up names in the other scripts)


### Set training period for JAGS model
training_date_start = '2015-04-03'
training_date_end = '2016-02-29'
last_date2plot = as.Date('2016-05-19')
###

### Set ensemble number and # of days to predict
n_ensemble = 5000 ## production run should be 200 - 5000, depending on what your computer can handle
n_forecast = 14 # number of days to forecast