require(ggplot2)
require(reshape2)
source('../utils.R')

x = read.csv("../../input/Load_history_augmented.csv", header=T)

#
x.2007 = x[which(x$year ==2007),]
x.2007.melt = melt(x.2007[,1:30], id.vars=1:10)
ggplot(x.2007.melt, aes(x=day_of_week, fill = variable, y = value)) + geom_bar(stat="identity") + facet_wrap(~season, ncol=2)
ggsave("TotalLoadByDayOfWeekAndSeason.png")

#
x.2007 = x[which(x$year ==2007),]
x.2007[,11:30] = scale(x.2007[,11:30])
x.2007.profile = ddply(x.2007, .variables=c('season', "day_of_week", "hour"), .fun=function(x) apply(x[11:30],2,mean))
x.2007.profile.melt = melt(x.2007.profile, id.vars=1:3)
ggplot(x.2007.profile.melt, aes(x=hour, y = value, group = variable, color = variable)) + geom_line() + facet_wrap(day_of_week~season, ncol=4)
ggsave("LoadProfileByDayOfWeekAndSeason.png")

#Plot ccf matrices
mts = x[-(1:38600),-(1:10)]
mts.ccf = ccf.matrix(mts, include0=F, na.action=na.pass)
write.csv(mts.ccf$ccfMatrix, "ccfMatrix.csv")
write.csv(mts.ccf$peakLag, "peakMatrix.csv")
mts.ccf = ccf.matrix(mts, include0=T, na.action=na.pass)
write.csv(mts.ccf$ccfMatrix, "ccfMatrix0.csv")
write.csv(mts.ccf$peakLag, "peakMatrix0.csv")

#Plot ccf matrices removing seasonality
mts.deseason = deseason(mts)
mts.deseason.ccf = ccf.matrix(mts.deseason, include0=F, na.action=na.pass)
write.csv(mts.deseason.ccf$ccfMatrix, "ccfMatrix.deseason.csv")
write.csv(mts.deseason.ccf$peakLag, "peakMatrix.deseason.csv")
mts.deseason.ccf = ccf.matrix(mts.deseason, include0=T, na.action=na.pass)
write.csv(mts.deseason.ccf$ccfMatrix, "ccfMatrix0.deseason.csv")
write.csv(mts.deseason.ccf$peakLag, "peakMatrix0.deseason.csv")

#Plot time series in ggplot
plotLoad = function(year, month, day) {
    x = x[which(x$year == year & x$month == month & x$day %in% day),]
    x.melt = melt(x[,1:30], id.vars=1:10)
    ggplot(x.melt, aes(x=timestamp, group = variable, y = value, color = variable)) + geom_line()
}
plotLoad(2005, 3, 1:5)
    

