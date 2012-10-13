require(ggplot2)
require(reshape2)

rawLoadData = read.csv('../../input/Load_history_training.csv', header=T,
                       colClasses = rep("numeric", 28))
rawTempData = read.csv('../../input/temperature_history.csv', header=T,
                       colClasses = rep("numeric", 28))
input = transform.fastVAR(rawLoadData, rawTempData)


missing.indices = which(rawLoadData$month == 3 & rawLoadData$day %in% 5:7)
missing.data = rawLoadData[missing.indices,]
#missing.melt = melt(missing.data, id.vars = colnames(missing.data)[1:4])
#ggplot(missing.melt, aes(y=value, group=zone_id)) + geom_line()


startIndex = nrow(input$load) - 24*7 + 1
endIndex = nrow(input$load)

training = input$load[startIndex:endIndex,]
training.melt = melt(training, id.vars=colnames(training))
ggplot(data=training.melt, aes(x=Var1, y=value, group=Var2, color=Var2)) + geom_line()
