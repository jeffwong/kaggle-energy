source('base.R')
require(forecast)

########################
#Impute using splines and lm
########################

imputed = do.call('rbind',lapply(missingPeriods, function(missingDates) {
    do.call('rbind', lapply(missingDates$day, function(day) {
        #Given a specific day, break it down by zone
        missing.indices = which(rawLoadData$month == missingDates$month & rawLoadData$day == day)
        imputed = ddply(rawLoadData[missing.indices,], c("zone_id"), function(d) {
            day.imputed = apply(d[,5:28], 2, function(j) {
                #interpolate the hours using lm
                x = 1:length(j)
                missing.index = which(is.na(j))
                predict(lm(j ~ ., data.frame(x=x)), newdata = data.frame(x=missing.index))
            })
            cbind(zone_id=d$zone_id[1], year=missingDates$year, month=missingDates$month, day=day,
                  data.frame(as.list(day.imputed)))
        })
        rbind(imputed, data.frame(zone_id=21, year=missingDates$year, month = missingDates$month,
              day = day, as.list(apply(imputed[,5:28],2,sum))))           
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
prediction.train = matrix(0, nrow = 24*7, ncol=20)
for (i in 1:20) {
  print(i)
  j = training.scaled[,i]
  j.model = auto.arima(ts(j, frequency=24), parallel=T)
  if (is.null(j.model$xreg)) 
    prediction.train[,i] = predict(j.model, n.ahead = 24*7)$pred
  else 
    prediction.train[,i] = predict(j.model, n.ahead = 24*7,
      newxreg=(nrow(training)+1) : (nrow(training) + nrow(validation)))$pred
}
colnames(prediction.train) = colnames(validation)

#Cross Validate
errors = (validation.scaled - prediction.train) / validation.scaled
RMSE = apply(errors, 2, function(j) sqrt(mean((j^2))))
barplot(RMSE)
png(filename="performance/naive3RMSE.png")
barplot(RMSE)
dev.off()

validation.melt = melt(validation.scaled)
prediction.train.melt = melt(prediction.train)
merged = merge(validation.melt, prediction.train.melt, by=c("Var1", "Var2"))
ggplot(merged, aes(x=Var1)) + geom_line(aes(y=value.x, color='red')) + geom_line(aes(y=value.y, color='green')) + facet_wrap(~Var2, ncol=5)
ggsave("performance/naive3matrix.png")

#Train final model
prediction.test = data.frame(apply(testing.scaled, 2, function(j) {
  j.model = auto.arima(ts(j, frequency=24), parallel=T)
  if (is.null(j.model$xreg)) 
    prediction.train[,i] = predict(j.model, n.ahead = 24*7)$pred
  else 
    prediction.train[,i] = predict(j.model, n.ahead = 24*7,
      newxreg=(nrow(training)+1) : (nrow(training) + nrow(validation)))$pred
}))

#Sanity Check
prediction.test.melt = melt(prediction.test)
ggplot(prediction.test.melt, aes(x=Var1, group=Var2, y=value, color=Var2)) + geom_line() + facet_wrap(~Var2, ncol=5)

#Rescale
for (i in 1:ncol(prediction.test)) {
  prediction.test[,i] = prediction.test[,i] * attr(testing.scaled, "scaled:scale")[i] + attr(testing.scaled, "scaled:center")[i]
}

#Prepare submission
prediction.output = cbind(zone_id=rep(1:20,each=7), do.call(rbind, lapply(prediction.test, function(i) {
  pred = matrix(i, ncol=24)
  data.frame(cbind(year=rep(2008,7), month = rep(7,7), day=1:7, pred))
})))
prediction.agg = ddply(prediction.output, "day", function(d) {
  cbind(zone_id = 21, year=2008, month=7,day=d$day[1], data.frame(as.list(apply(d[,5:28], 2, sum))))
})
prediction.output = rbind(prediction.output, prediction.agg)
prediction.output = prediction.output[order(prediction.output$day, prediction.output$zone_id),]
colnames(prediction.output)[5:28] = paste("h", 1:24, sep="")
final = rbind(imputed, prediction.output)
final = cbind(id=1:nrow(final), final)
write.csv(final, "../../submissions/submission3.csv", row.names=F, quote=F)
