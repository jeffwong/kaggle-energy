require(forecast)
require(reshape2)
require(ggplot2)
source("../utils.R")

########################
#Impute using means
########################

rawLoadData = read.csv('../../input/Load_history_training.csv', header=T,
                       colClasses = rep("numeric", 28))

missing1 = do.call(rbind, lapply(6:12, function(day) {
    missing.indices = which(rawLoadData$month == 3 & rawLoadData$day == day)
    averages = ddply(rawLoadData[missing.indices,], c("zone_id"), function(d) apply(d,2,mean, na.rm=T))
    averages = rbind(averages,
        data.frame(zone_id=21, year=2005, month = 3, day = day, as.list(apply(averages[,5:28],2,sum))))
    averages$year = 2005
    averages
}))
missing2 = do.call(rbind, lapply(20:26, function(day) {
    missing.indices = which(rawLoadData$month == 6 & rawLoadData$day == day)
    averages = ddply(rawLoadData[missing.indices,], c("zone_id"), function(d) apply(d,2,mean, na.rm=T))
    averages = rbind(averages,
        data.frame(zone_id=21, year=2005, month = 6, day = day, as.list(apply(averages[,5:28],2,sum))))
    averages$year = 2005
    averages
}))
missing3 = do.call(rbind, lapply(10:16, function(day) {
    missing.indices = which(rawLoadData$month == 9 & rawLoadData$day == day)
    averages = ddply(rawLoadData[missing.indices,], c("zone_id"), function(d) apply(d,2,mean, na.rm=T))
    averages = rbind(averages,
        data.frame(zone_id=21, year=2005, month = 9, day = day, as.list(apply(averages[,5:28],2,sum))))
    averages$year = 2005
    averages
}))
missing4 = do.call(rbind, lapply(25:31, function(day) {
    missing.indices = which(rawLoadData$month == 12 & rawLoadData$day == day)
    averages = ddply(rawLoadData[missing.indices,], c("zone_id"), function(d) apply(d,2,mean, na.rm=T))
    averages = rbind(averages,
        data.frame(zone_id=21, year=2005, month = 12, day = day, as.list(apply(averages[,5:28],2,sum))))
    averages$year = 2005
    averages
}))
missing5 = do.call(rbind, lapply(13:19, function(day) {
    missing.indices = which(rawLoadData$month == 2 & rawLoadData$day == day)
    averages = ddply(rawLoadData[missing.indices,], c("zone_id"), function(d) apply(d,2,mean, na.rm=T))
    averages = rbind(averages,
        data.frame(zone_id=21, year=2006, month = 2, day = day, as.list(apply(averages[,5:28],2,sum))))
    averages$year = 2006
    averages
}))
missing6 = do.call(rbind, lapply(25:31, function(day) {
    missing.indices = which(rawLoadData$month == 5 & rawLoadData$day == day)
    averages = ddply(rawLoadData[missing.indices,], c("zone_id"), function(d) apply(d,2,mean, na.rm=T))
    averages = rbind(averages,
        data.frame(zone_id=21, year=2006, month = 5, day = day, as.list(apply(averages[,5:28],2,sum))))
    averages$year = 2006
    averages
}))
missing7 = do.call(rbind, lapply(2:8, function(day) {
    missing.indices = which(rawLoadData$month == 8 & rawLoadData$day == day)
    averages = ddply(rawLoadData[missing.indices,], c("zone_id"), function(d) apply(d,2,mean, na.rm=T))
    averages = rbind(averages,
        data.frame(zone_id=21, year=2006, month = 8, day = day, as.list(apply(averages[,5:28],2,sum))))
    averages$year = 2006
    averages
}))
missing8 = do.call(rbind, lapply(22:28, function(day) {
    missing.indices = which(rawLoadData$month == 11 & rawLoadData$day == day)
    averages = ddply(rawLoadData[missing.indices,], c("zone_id"), function(d) apply(d,2,mean, na.rm=T))
    averages = rbind(averages,
        data.frame(zone_id=21, year=2006, month = 11, day = day, as.list(apply(averages[,5:28],2,sum))))
    averages$year = 2006
    averages
}))
imputed = rbind(missing1, missing2, missing3, missing4, missing5, missing6, missing7, missing8)

#######################
#Predict using forecast
#######################

rawTempData = read.csv('../../input/temperature_history.csv', header=T,
                       colClasses = rep("numeric", 28))
input = transform.fastVAR(rawLoadData, rawTempData)

#Vary algorithm here
startIndex = nrow(input$load) - 24*7 + 1
endIndex = nrow(input$load)

prediction = apply(input$load[startIndex:endIndex,], 2, function(j) {
    predict(auto.arima(ts(j)), 24*7)
})
prediction.output = data.frame(cbind(zone_id=rep(1:20,each=7), do.call(rbind, lapply(prediction, function(i) {
    pred = matrix(i[[1]], ncol=24)
    cbind(year=2008, month = 7, day=1:7, pred)
}))))
#End Vary

prediction.agg = ddply(prediction.output, "day", function(d) {
    cbind(zone_id = 21, year=2008, month=7,day=d$day[1], data.frame(as.list(apply(d[,5:28], 2, sum))))
})
prediction.output = rbind(prediction.output, prediction.agg)

prediction.output = prediction.output[order(prediction.output$day, prediction.output$zone_id),]
colnames(prediction.output)[5:28] = paste("h", 1:24, sep="")

final = rbind(imputed, prediction.output)
final = cbind(id=1:nrow(final), final)
write.csv(final, "../../submissions/submission1.csv", row.names=F, quote=F)
