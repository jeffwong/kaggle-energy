require(gbm)
source("base.R")

rawLoadData.augmented = read.csv('../../input/Load_history_augmented.csv', header=T)[-(39415:39600),]
rawLoadData.augmented[,4] = as.factor(rawLoadData.augmented[,4])
rawLoadData.augmented[,5] = as.factor(rawLoadData.augmented[,5])
rawLoadData.augmented[,6] = as.factor(rawLoadData.augmented[,6])
rawLoadData.imputed = rawLoadData.augmented

missing.indices = which(is.na(rawLoadData.augmented[,11]))
missing.matrix = matrix(missing.indices, ncol=8)
missing.rows = list(
    missing1 = missing.matrix[,1],
    missing2 = missing.matrix[,2],
    missing3 = missing.matrix[,3],
    missing4 = missing.matrix[,4],
    missing5 = missing.matrix[,5],
    missing6 = missing.matrix[,6],
    missing7 = missing.matrix[,7],
    missing8 = missing.matrix[,8]
)

########################
#Impute using gbm
########################

imputed = lapply(missing.rows, function(i) {
    apply(rawLoadData.augmented[,11:30], 2, function(j) {
        missing.indices.test.start = i[1]
        missing.indices.test.end = i[168]
        missing.indices.test = i
    
        missing.indices.validation.end = missing.indices.test.start - 1
        missing.indices.validation.start = missing.indices.validation.end - 24*7 + 1
        missing.indices.validation = missing.indices.validation.start : missing.indices.validation.end

        missing.indices.train.end = missing.indices.validation.start - 1
        missing.indices.train.start = missing.indices.train.end - 24*7*8 + 1
        missing.indices.train = missing.indices.train.start : missing.indices.train.end
    
        y = j
        y.test = y[missing.indices.test]
        y.validation = y[missing.indices.validation]
        y.train = y[missing.indices.train]
        bad.indices = which(is.na(y.train))
        x = rawLoadData.augmented[missing.indices.train,]
        if(length(bad.indices) > 0) {
            y.train = y.train[-bad.indices]
            x = x[-bad.indices,]
        }
    
        #Train and CV
        y.gbm = gbm(y.train ~ year+month+day+holiday+weekend+
                    business_day+season+day_of_week+hour+temp1+temp2+temp3+temp4+temp5+
                    temp6+temp7+temp8+temp9+temp10+temp11,
                    data = x,
                    distribution="gaussian", n.trees=1500,
                    var.monotone = rep(0, 20),
                    shrinkage = 0.005,
                    interaction.depth = 3,
                    cv.folds = 3)
        best.iter = gbm.perf(y.gbm, method="OOB", plot.it =F)
        y.pred = predict(y.gbm, rawLoadData.augmented[missing.indices.validation,], best.iter)
        errors = y.validation - y.pred
    
        predict(y.gbm, rawLoadData.augmented[missing.indices.test,], best.iter)
    })
})
imputed = do.call('rbind', imputed)
rawLoadData.imputed[missing.indices,11:30] = imputed

#Prepare imputation output
mdylabels = do.call('rbind', lapply(missingPeriods, function(i) {
    data.frame(year = i$year, month = i$month, day = i$day)
}))
imputed.output = do.call('rbind', lapply(11:30, function(i) {
    j = rawLoadData.imputed[missing.indices,i]
    cbind(zone_id = (i-10), mdylabels, matrix(j, ncol=24, byrow=T))
}))
imputed.agg = ddply(imputed.output, c("year", "month", "day"), .fun = function(d) {
    data.frame(zone_id = 21, year = d$year[1], month = d$month[1],
               day = d$day[1], matrix(apply(d[,-(1:4)],2,sum), nrow=1))
})
colnames(imputed.output)[5:28] = paste("h", 1:24, sep="")
colnames(imputed.agg)[5:28] = paste("h", 1:24, sep="")
imputed.output = rbind(imputed.output, imputed.agg)
imputed.output = imputed.output[order(imputed.output$year,
                                      imputed.output$month,
                                      imputed.output$day,
                                      imputed.output$zone_id),]



#########
#Forecast
#########

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
png(filename="performance/gbmRMSE.png")
barplot(RMSE)
dev.off()

validation.melt = melt(validation.scaled)
prediction.train.melt = melt(prediction.train)
merged = merge(validation.melt, prediction.train.melt, by=c("Var1", "Var2"))
ggplot(merged, aes(x=Var1)) + geom_line(aes(y=value.x, color='red')) + geom_line(aes(y=value.y, color='green')) + facet_wrap(~Var2, ncol=5)
ggsave("performance/gbmmatrix.png")

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
  pred = matrix(i, ncol=24, byrow=T)
  data.frame(cbind(year=rep(2008,7), month = rep(7,7), day=1:7, pred))
})))
prediction.agg = ddply(prediction.output, "day", function(d) {
  cbind(zone_id = 21, year=2008, month=7,day=d$day[1], data.frame(as.list(apply(d[,5:28], 2, sum))))
})
prediction.output = rbind(prediction.output, prediction.agg)
prediction.output = prediction.output[order(prediction.output$day, prediction.output$zone_id),]
colnames(prediction.output)[5:28] = paste("h", 1:24, sep="")
final = rbind(imputed.output, prediction.output)
final = cbind(id=1:nrow(final), final)
write.csv(final, "../../submissions/submission6.csv", row.names=F, quote=F)
