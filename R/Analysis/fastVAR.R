require(devtools)
install_github("fastVAR", "jeffwong")
require(fastVAR)
source("../utils.R")

######################
#Predict using fastVAR
######################

rawLoadData = read.csv('../../input/Load_history_training.csv', header=T,
                       colClasses = rep("numeric", 28)) 
rawTempData = read.csv('../../input/temperature_history.csv', header=T,
                       colClasses = rep("numeric", 28))
input = transform.fastVAR(rawLoadData, rawTempData)

load.var = fastVAR(input$load[,-7], 1, getdiag=F)
load.predict = VARpredict(input$load[,-7], 1, load.var$coefficients, 24*7)
