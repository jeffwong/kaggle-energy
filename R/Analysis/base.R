require(reshape2)
require(ggplot2)
source("../utils.R")

rawLoadData = read.csv('../../input/Load_history_training.csv', header=T,
                       colClasses = rep("numeric", 28))
rawTempData = read.csv('../../input/temperature_history.csv', header=T,
                       colClasses = rep("numeric", 28))

missingPeriods = list(
        list(year = 2005, month = 3, day = 6:12),
        list(year = 2005, month = 6, day = 20:26),
        list(year = 2005, month = 9, day = 10:16),
        list(year = 2005, month = 12, day = 25:31),
        list(year = 2006, month = 2, day = 13:19),
        list(year = 2006, month = 5, day = 25:31),
        list(year = 2006, month = 8, day = 2:8),
        list(year = 2006, month = 11, day = 22:28)
    )



