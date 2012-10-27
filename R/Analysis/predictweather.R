require(gbm)
source("base.R")

rawLoadData.augmented = read.csv('../../input/Load_history_augmented.csv', header=T)
rawLoadData.augmented[,4] = as.factor(rawLoadData.augmented[,4])
rawLoadData.augmented[,5] = as.factor(rawLoadData.augmented[,5])
rawLoadData.augmented[,6] = as.factor(rawLoadData.augmented[,6])

rawLoadData.augmented.current = rawLoadData.augmented[-(39415:39600),]
rawLoadData.augmented.future = rawLoadData.augmented[39415:39600,]

training.size = 24*14
testing.size = 24*14
validation.size = 24*7

validation.end = nrow(rawLoadData.augmented.current)
validation.start = validation.end - validation.size + 1
training.end = validation.start - 1
training.start = training.end - training.size + 1
testing.end = nrow(rawLoadData.augmented.current)
testing.start = testing.end - testing.size + 1

validation = rawLoadData.augmented.current[validation.start : validation.end,]
training = rawLoadData.augmented.current[training.start : training.end,]
testing = rawLoadData.augmented.current[testing.start : testing.end,]

temp.predict = do.call('cbind', lapply(1:11, function(i) {
    y.gbm = gbm(training[,30+i] ~ month + day + holiday + weekend + business_day +
                                  season + day_of_week + hour,
                                  data = training,
                                  distribution="gaussian", n.trees=1500,
                                  var.monotone = rep(0, 8),
                                  shrinkage = 0.005,
                                  interaction.depth = 3,
                                  cv.folds = 3)
    best.iter = gbm.perf(y.gbm, method="OOB", plot.it =F)
    y.pred = predict(y.gbm, validation, best.iter)
    errors = validation[,30+i] - y.pred
    
    predict(y.gbm, rawLoadData.augmented.future, best.iter)
}))
colnames(temp.predict) = colnames(validation)[31:41]
save(temp.predict, file="temp.predict.Rdata")

    

