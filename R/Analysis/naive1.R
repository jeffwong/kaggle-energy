source('base.R')
require(forecast)

########################
#Impute using means
########################

imputed = do.call('rbind',lapply(missingPeriods, function(missingDates) {
    do.call('rbind', lapply(missingDates$day, function(day) {
        #Given a specific day, break it down by zone
        missing.indices = which(rawLoadData$month == missingDates$month & rawLoadData$day == day)
        averages = ddply(rawLoadData[missing.indices,], c("zone_id"), function(d) apply(d,2,mean, na.rm=T))
        averages = rbind(averages,
            data.frame(zone_id=21, year=missingDates$year, month = missingDates$month, day = day, as.list(apply(averages[,5:28],2,sum))))
        averages$year = missingDates$year
        averages
    }))
}))

#######################
#Predict using forecast
#######################

input = transform.fastVAR(rawLoadData, rawTempData)

validation.end = nrow(input$load) - 24
validation.start = validation.end - 24*7 + 1
training.end = validation.start - 1
training.start = training.end - 24*14 + 1
testing.end = nrow(input$load)
testing.start = testing.end - 24*14 + 1

validation = input$load[validation.start : validation.end,]
training = input$load[training.start : training.end,]
testing = input$load[testing.start : testing.end,]

training.scaled = scale(training)
validation.scaled = scale(validation, center = attr(training.scaled, "scaled:center"),
                          scale = attr(training.scaled, "scaled:scale"))
testing.scaled = scale(testing)



#Vary algorithm here
prediction.train = apply(training.scaled, 2, function(j) {
  j.model = auto.arima(ts(j), parallel=T)
  if (is.null(j.model$xreg)) 
    predict(j.model, n.ahead = 24*7)$pred
  else 
    predict(j.model, n.ahead = 24*7,
      newxreg=(nrow(training)+1) : (nrow(training) + nrow(validation)))$pred
})
colnames(prediction.train) = colnames(validation)

#Cross Validate
errors = (validation.scaled - prediction.train) / validation.scaled
RMSE = apply(errors, 2, function(j) sqrt(mean((j^2))))
barplot(RMSE)
png(filename="performance/naive1RMSE.png")
barplot(RMSE)
dev.off()

validation.melt = melt(validation.scaled)
prediction.train.melt = melt(prediction.train)
merged = merge(validation.melt, prediction.train.melt, by=c("Var1", "Var2"))
ggplot(merged, aes(x=Var1)) + geom_line(aes(y=value.x, color='red')) + geom_line(aes(y=value.y, color='green')) + facet_wrap(~Var2, ncol=5)
ggsave("performance/naive1matrix.png")

#Train final model
prediction.test = apply(testing.scaled, 2, function(j) {
  j.model = auto.arima(ts(j), parallel=T)
  if (is.null(j.model$xreg)) 
    predict(j.model, n.ahead = 24*7)$pred
  else 
    predict(j.model, n.ahead = 24*7,
      newxreg=(nrow(training)+1) : (nrow(training) + nrow(validation)))$pred
})

#Sanity Check
prediction.test.melt = melt(prediction.test)
ggplot(prediction.test.melt, aes(x=Var1, group=Var2, y=value, color=Var2)) + geom_line() + facet_wrap(~Var2, ncol=5)

#Rescale
for (i in 1:ncol(prediction.test)) {
  prediction.test[,i] = prediction.test[,i] * attr(testing.scaled, "scaled:scale")[i] + attr(testing.scaled, "scaled:center")[i]
}

#Prepare submission
prediction.output = data.frame(cbind(zone_id=rep(1:20,each=7), do.call(rbind, lapply(data.frame(prediction.test), function(i) {
    pred = matrix(i, ncol=24)
    cbind(year=2008, month = 7, day=1:7, pred)
}))))
prediction.agg = ddply(prediction.output, "day", function(d) {
    cbind(zone_id = 21, year=2008, month=7,day=d$day[1], data.frame(as.list(apply(d[,5:28], 2, sum))))
})
prediction.output = rbind(prediction.output, prediction.agg)
prediction.output = prediction.output[order(prediction.output$day, prediction.output$zone_id),]
colnames(prediction.output)[5:28] = paste("h", 1:24, sep="")
final = rbind(imputed, prediction.output)
final = cbind(id=1:nrow(final), final)
write.csv(final, "../../submissions/submission1b.csv", row.names=F, quote=F)
