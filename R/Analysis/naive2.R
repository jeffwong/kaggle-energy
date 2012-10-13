require(forecast)
require(reshape2)
require(ggplot2)
source("../utils.R")

rawLoadData = read.csv('../../input/Load_history_training.csv', header=T,
                       colClasses = rep("numeric", 28))
rawTempData = read.csv('../../input/temperature_history.csv', header=T,
                       colClasses = rep("numeric", 28))

########################
#Impute using means
########################

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
#Predict using persistence
#######################

input = transform.fastVAR(rawLoadData, rawTempData)

validation.end = nrow(input$load) - 24
validation.start = validation.end - 24*7 + 1
training.end = validation.start - 1
training.start = training.end - 24*7 + 1
testing.end = nrow(input$load)
testing.start = testing.end - 24*7 + 1

validation = input$load[validation.start : validation.end,]
training = input$load[training.start : training.end,]
testing = input$load[testing.start : testing.end,]

training.scaled = scale(training)
validation.scaled = scale(validation, center = attr(training.scaled, "scaled:center"),
                          scale = attr(training.scaled, "scaled:scale"))
testing.scaled = scale(testing)



#Vary algorithm here
prediction.train = apply(training.scaled, 2, identity)
colnames(prediction.train) = colnames(validation)

#Cross Validate
errors = (validation.scaled - prediction.train) / validation.scaled
RMSE = apply(errors, 2, function(j) sqrt(mean((j^2))))
barplot(RMSE)
png(filename="performance/naive2RMSE.png")
barplot(RMSE)
dev.off()

validation.melt = melt(validation.scaled)
prediction.train.melt = melt(prediction.train)
merged = merge(validation.melt, prediction.train.melt, by=c("Var1", "Var2"))
ggplot(merged, aes(x=Var1)) + geom_line(aes(y=value.x, color='red')) + geom_line(aes(y=value.y, color='green')) + facet_wrap(~Var2, ncol=5)
ggsave("performance/naive2matrix.png")

#Train final model
prediction.test = apply(testing.scaled, 2, function(j) {
    pred = j
    pred[151:168] = pred[(151-24) : (168-24)]
    pred
})

#Sanity Check
prediction.test.melt = melt(prediction.test)
ggplot(prediction.test.melt, aes(x=Var1, group=Var2, y=value, color=Var2)) + geom_line() + facet_wrap(~Var2, ncol=5)

#Rescale
for (i in 1:ncol(prediction.test)) {
  prediction.test[,i] = prediction.test[,i] * attr(testing.scaled, "scaled:scale")[i] + attr(testing.scaled, "scaled:center")[i]
}

#Prepare submission
prediction.output = cbind(zone_id=rep(1:20,each=7), do.call(rbind, apply(prediction.test, 2, function(j) {
    pred = matrix(j, ncol=24)
    data.frame(cbind(year=2008, month = 7, day=1:7, pred))
})))
prediction.agg = ddply(prediction.output, "day", function(d) {
    cbind(zone_id = 21, year=2008, month=7,day=d$day[1], data.frame(as.list(apply(d[,5:28], 2, sum))))
})
prediction.output = rbind(prediction.output, prediction.agg)
prediction.output = prediction.output[order(prediction.output$day, prediction.output$zone_id),]
colnames(prediction.output)[5:28] = paste("h", 1:24, sep="")
final = rbind(imputed, prediction.output)
final = cbind(id=1:nrow(final), final)
write.csv(final, "../../submissions/submission2.csv", row.names=F, quote=F)
