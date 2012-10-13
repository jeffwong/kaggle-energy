require(forecast)
require(reshape2)
require(ggplot2)
source("../utils.R")



rawLoadData = read.csv('../../input/Load_history_training.csv', header=T,
                       colClasses = rep("numeric", 28))
rawTempData = read.csv('../../input/temperature_history.csv', header=T,
                       colClasses = rep("numeric", 28))

########################
#Impute using splines and lm
########################

missing1 = do.call(rbind, lapply(6:12, function(day) {
  missing.indices = which(rawLoadData$month == 3 & rawLoadData$day == day)
  imputed = ddply(rawLoadData[missing.indices,], c("zone_id"), function(d) {
    day.imputed = apply(d[,5:28], 2, function(j) {
      x = 1:length(j)
      missing.index = which(is.na(j))
      predict(smooth.spline(x[-missing.index], j[-missing.index]), missing.index)$y
    })
    cbind(zone_id=d$zone_id[1], year=2005, month=d$month[1], day=day,
          data.frame(as.list(day.imputed)))
  })
  rbind(imputed, data.frame(zone_id=21, year=imputed$year[1], month = imputed$month[1],
          day = imputed$day[1], as.list(apply(imputed[,5:28],2,sum))))
}))
missing2 = do.call(rbind, lapply(20:26, function(day) {
  missing.indices = which(rawLoadData$month == 6 & rawLoadData$day == day)
  imputed = ddply(rawLoadData[missing.indices,], c("zone_id"), function(d) {
    day.imputed = apply(d[,5:28], 2, function(j) {
      x = 1:length(j)
      missing.index = which(is.na(j))
      predict(smooth.spline(x[-missing.index], j[-missing.index]), missing.index)$y
    })
    cbind(zone_id=d$zone_id[1], year=2005, month=d$month[1], day=day,
          data.frame(as.list(day.imputed)))
  })
  rbind(imputed, data.frame(zone_id=21, year=imputed$year[1], month = imputed$month[1],
          day = imputed$day[1], as.list(apply(imputed[,5:28],2,sum))))
}))
missing3 = do.call(rbind, lapply(10:16, function(day) {
  missing.indices = which(rawLoadData$month == 9 & rawLoadData$day == day)
  imputed = ddply(rawLoadData[missing.indices,], c("zone_id"), function(d) {
    day.imputed = apply(d[,5:28], 2, function(j) {
      x = 1:length(j)
      missing.index = which(is.na(j))
      predict(lm(j ~ ., data.frame(x=x)), newdata = data.frame(x=missing.index))
    })
    cbind(zone_id=d$zone_id[1], year=2005, month=d$month[1], day=day,
          data.frame(as.list(day.imputed)))
  })
  rbind(imputed, data.frame(zone_id=21, year=imputed$year[1], month = imputed$month[1],
          day = imputed$day[1], as.list(apply(imputed[,5:28],2,sum))))
}))
missing4 = do.call(rbind, lapply(25:31, function(day) {
  missing.indices = which(rawLoadData$month == 12 & rawLoadData$day == day)
  imputed = ddply(rawLoadData[missing.indices,], c("zone_id"), function(d) {
    day.imputed = apply(d[,5:28], 2, function(j) {
      x = 1:length(j)
      missing.index = which(is.na(j))
      predict(lm(j ~ ., data.frame(x=x)), newdata = data.frame(x=missing.index))
    })
    cbind(zone_id=d$zone_id[1], year=2005, month=d$month[1], day=day,
          data.frame(as.list(day.imputed)))
  })
  rbind(imputed, data.frame(zone_id=21, year=imputed$year[1], month = imputed$month[1],
          day = imputed$day[1], as.list(apply(imputed[,5:28],2,sum))))
}))
missing5 = do.call(rbind, lapply(13:19, function(day) {
  missing.indices = which(rawLoadData$month == 2 & rawLoadData$day == day)
  imputed = ddply(rawLoadData[missing.indices,], c("zone_id"), function(d) {
    day.imputed = apply(d[,5:28], 2, function(j) {
      x = 1:length(j)
      missing.index = which(is.na(j))
      predict(lm(j ~ ., data.frame(x=x)), newdata = data.frame(x=missing.index))
    })
    cbind(zone_id=d$zone_id[1], year=2006, month=d$month[1], day=day,
          data.frame(as.list(day.imputed)))
  })
  rbind(imputed, data.frame(zone_id=21, year=imputed$year[1], month = imputed$month[1],
          day = imputed$day[1], as.list(apply(imputed[,5:28],2,sum))))
}))
missing6 = do.call(rbind, lapply(25:31, function(day) {
  missing.indices = which(rawLoadData$month == 5 & rawLoadData$day == day)
  imputed = ddply(rawLoadData[missing.indices,], c("zone_id"), function(d) {
    day.imputed = apply(d[,5:28], 2, function(j) {
      x = 1:length(j)
      missing.index = which(is.na(j))
      predict(lm(j ~ ., data.frame(x=x)), newdata = data.frame(x=missing.index))
    })
    cbind(zone_id=d$zone_id[1], year=2006, month=d$month[1], day=day,
          data.frame(as.list(day.imputed)))
  })
  rbind(imputed, data.frame(zone_id=21, year=imputed$year[1], month = imputed$month[1],
          day = imputed$day[1], as.list(apply(imputed[,5:28],2,sum))))
}))
missing7 = do.call(rbind, lapply(2:8, function(day) {
  missing.indices = which(rawLoadData$month == 8 & rawLoadData$day == day)
  imputed = ddply(rawLoadData[missing.indices,], c("zone_id"), function(d) {
    day.imputed = apply(d[,5:28], 2, function(j) {
      x = 1:length(j)
      missing.index = which(is.na(j))
      predict(lm(j ~ ., data.frame(x=x)), newdata = data.frame(x=missing.index))
    })
    cbind(zone_id=d$zone_id[1], year=2006, month=d$month[1], day=day,
          data.frame(as.list(day.imputed)))
  })
  rbind(imputed, data.frame(zone_id=21, year=imputed$year[1], month = imputed$month[1],
          day = imputed$day[1], as.list(apply(imputed[,5:28],2,sum))))
}))
missing8 = do.call(rbind, lapply(22:28, function(day) {
  missing.indices = which(rawLoadData$month == 11 & rawLoadData$day == day)
  imputed = ddply(rawLoadData[missing.indices,], c("zone_id"), function(d) {
    day.imputed = apply(d[,5:28], 2, function(j) {
      x = 1:length(j)
      missing.index = which(is.na(j))
      predict(lm(j ~ ., data.frame(x=x)), newdata = data.frame(x=missing.index))
    })
    cbind(zone_id=d$zone_id[1], year=2006, month=d$month[1], day=day,
          data.frame(as.list(day.imputed)))
  })
  rbind(imputed, data.frame(zone_id=21, year=imputed$year[1], month = imputed$month[1],
          day = imputed$day[1], as.list(apply(imputed[,5:28],2,sum))))
}))
imputed = rbind(missing1, missing2, missing3, missing4, missing5, missing6, missing7, missing8)

#######################
#Predict using forecast
#######################

input = transform.fastVAR(rawLoadData, rawTempData)

training.size = 24*14
testing.size = 24*14

validation.end = nrow(input$load) - 24
validation.start = validation.end - 24*7 + 1
training.end = validation.start - 1
training.start = training.end - training.size + 1
testing.end = nrow(input$load)
testing.start = testing.end - testing.size + 1

validation = input$load[validation.start : validation.end,]
training = input$load[training.start : training.end,]
testing = input$load[testing.start : testing.end,]

training.scaled = scale(training)
validation.scaled = scale(validation, center = attr(training.scaled, "scaled:center"),
                          scale = attr(training.scaled, "scaled:scale"))
testing.scaled = scale(testing)

for (i in 1:ncol(testing.scaled)) {
    x = training.scaled[(training.size - 24 + 1) : training.size,i]
    y = testing.scaled[(testing.size - 24 + 1) : testing.size,i]
    imputing.lm = lm (y[1:6] ~ ., data=data.frame(x = x[1:6]))
    y.impute = predict(imputing.lm, newdata=data.frame(x = x[-(1:6)]))
    testing.scaled[(testing.size - 24 + 1) : testing.size,i][7:24] = y.impute
}

#Vary algorithm here
prediction.train = matrix(0, nrow = 24*7, ncol=20)
for (i in 1:20) {
  print(i)
  j = training.scaled[,i]
  j.ts = ts(j, frequency=24)
  j.stl = stl(j.ts, s.window = "periodic")
  j.periodic = j.stl$time.series[(training.size - 24*7 + 1) : training.size,1]
  y = apply(j.stl$time.series[,-1], 1, sum)
  j.model = auto.arima(ts(y), parallel=T, stationary=T, allowdrift=F)
  if (is.null(j.model$xreg)) 
    prediction.train[,i] = predict(j.model, n.ahead = 24*7)$pred + j.periodic
  else 
    prediction.train[,i] = predict(j.model, n.ahead = 24*7,
      newxreg=(nrow(training)+1) : (nrow(training) + nrow(validation)))$pred + j.periodic
}
colnames(prediction.train) = colnames(validation)

#Cross Validate
errors = (validation.scaled - prediction.train) / validation.scaled
RMSE = apply(errors, 2, function(j) sqrt(mean((j^2))))
barplot(RMSE)
png(filename="performance/naive4RMSE.png")
barplot(RMSE)
dev.off()

validation.melt = melt(validation.scaled)
prediction.train.melt = melt(prediction.train)
merged = merge(validation.melt, prediction.train.melt, by=c("Var1", "Var2"))
ggplot(merged, aes(x=Var1)) + geom_line(aes(y=value.x, color='red')) + geom_line(aes(y=value.y, color='green')) + facet_wrap(~Var2, ncol=5)
ggsave("performance/naive4matrix.png")

#Train final model
prediction.test = apply(testing.scaled, 2, function(j) {
  j.ts = ts(j, frequency=24)
  j.stl = stl(j.ts, s.window = "periodic")
  j.periodic = j.stl$time.series[(testing.size - 24*7 + 1) : testing.size,1]
  y = apply(j.stl$time.series[,-1], 1, sum)
  j.model = auto.arima(ts(y), parallel=T, stationary=T, allowdrift=F)
  if (is.null(j.model$xreg)) 
    predict(j.model, n.ahead = 24*7)$pred + j.periodic
  else 
    predict(j.model, n.ahead = 24*7,
            newxreg=(nrow(training)+1) : (nrow(training) + nrow(validation)))$pred + j.periodic
})
prediction.test[,9] = testing.scaled[(testing.size - 24*7 + 1) : testing.size,9]

#Sanity Check
prediction.test.melt = melt(prediction.test)
ggplot(prediction.test.melt, aes(x=Var1, group=Var2, y=value, color=Var2)) + geom_line() + facet_wrap(~Var2, ncol=5)

#Rescale
for (i in 1:ncol(prediction.test)) {
  prediction.test[,i] = prediction.test[,i] * attr(testing.scaled, "scaled:scale")[i] + attr(testing.scaled, "scaled:center")[i]
}

#Prepare submission
prediction.output = cbind(zone_id=rep(1:20,each=7), do.call(rbind, lapply(data.frame(prediction.test), function(i) {
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
write.csv(final, "../../submissions/submission4.csv", row.names=F, quote=F)
