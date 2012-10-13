require(rpart)
source("../utils.R")

########################
#Impute using rpart
########################

rawLoadData = read.csv('../../input/Load_history_augmented.csv', header=T)

set.seed(100)

missing.indices = which(is.na(rawLoadData$load1))
load.rf = rpart(load1 ~ . , data=rawLoadData[,-10], method="anova")



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

rawTempData = read.csv('../../input/temperature_history.csv', header=T,
                       colClasses = rep("numeric", 28))
input = transform.fastVAR(rawLoadData, rawTempData)

testing.end = nrow(input$load) - 24
testing.start = testing.end - 24*7 + 1
training.end = testing.start - 1
training.start = training.end - 24*28 + 1
training = input$load[training.start : training.end, ]
testing = input$load[testing.start : testing.end,]

predict.end = testing.end
predict.start = predict.end - 24*28 + 1


#Vary algorithm here

#Predict
prediction = matrix(0, nrow = 24*7, ncol=20)
for (i in 1:20) {
  print(i)
  j = training[,i]
  prediction[,i] = predict(auto.arima(ts(j, frequency=24)), n.ahead = 24*7)$pred
}

#Cross Validate
errors = testing - prediction
test = apply(errors, 2, function(j) {
  sum(j^2)
})

#Create model for test set
prediction = data.frame(apply(input$load[predict.start:predict.end,], 2, function(j) {
  predict(auto.arima(ts(j, frequency=24)), n.ahead=24*7)$pred
}))


                   
#Format output                   
prediction.output = cbind(zone_id=rep(1:20,each=7), do.call(rbind, lapply(prediction, function(i) {
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
