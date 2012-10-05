require(reshape2)
require(timeDate)
require(lubridate)
require(plyr)

getSeason <- function(DATES) {
    WS <- as.Date("2012-12-15", format = "%Y-%m-%d") # Winter Solstice
    SE <- as.Date("2012-3-15",  format = "%Y-%m-%d") # Spring Equinox
    SS <- as.Date("2012-6-15",  format = "%Y-%m-%d") # Summer Solstice
    FE <- as.Date("2012-9-15",  format = "%Y-%m-%d") # Fall Equinox

    # Convert dates from any year to 2012 dates
    d <- as.Date(strftime(DATES, format="2012-%m-%d"))

    ifelse (d >= WS | d < SE, "Winter",
      ifelse (d >= SE & d < SS, "Spring",
        ifelse (d >= SS & d < FE, "Summer", "Fall")))
}

energy = read.csv("Load_history_training.csv", header=T)
energy.dates = apply(energy, 1, function(i) {
    paste(i[2:4], collapse='-')
})

#Identify Holidays

holidays = holiday(year = min(energy[,2]):max(energy[,2]), Holiday = listHolidays("US")[c(1,2,8,9,11,12,13,15,16)])
energy.dates = timeDate(energy.dates, format="%Y-%m-%d")
energy.holidays = which(isHoliday(energy.dates, holidays, 0:6))
energy$holiday = F
energy$holiday[energy.holidays] = T

#Identify Weekends

energy.weekends = which(isHoliday(energy.dates, c(), 1:5))
energy$weekend = F
energy$weekend[energy.weekends] = T

#Identify Business Days

energy$business_day = T
energy$business_day[which(isHoliday(energy.dates, holidays, 1:5))] = F

#Identify Season

energy$season = getSeason(energy.dates)

#Identify day of week

energy$day_of_week = wday(energy.dates, label=T)

#Format into multivariate time series

energy.zones = lapply(split(energy, energy$zone_id), function(zone) {
  energy.melt = melt(zone[,-1], id.vars=c(1:3, 28:32))
  energy.melt$variable = as.numeric(energy.melt$variable)
  names(energy.melt)[9] = "hour"
  names(energy.melt)[10] = paste("load", zone[1,1], sep="")
  return (energy.melt)
})
energy.mts = Reduce(function(...) merge(...,all=T), energy.zones)

#Process temp dataset

temp = read.csv("temperature_history.csv", header=T)
temp.dates = apply(temp, 1, function(i) {
    paste(i[2:4], collapse='-')
})
temp.dates = timeDate(temp.dates, format="%Y-%m-%d")
temp.holidays = which(isHoliday(temp.dates, holidays, 0:6))
temp$holiday = F
temp$holiday[temp.holidays] = T

temp.weekends = which(isHoliday(temp.dates, c(), 1:5))
temp$weekend = F
temp$weekend[temp.weekends] = T

temp$business_day = T
temp$business_day[which(isHoliday(temp.dates, holidays, 1:5))] = F

temp$season = getSeason(temp.dates)

temp$day_of_week = wday(temp.dates, label=T)

temp.zones = lapply(split(temp, temp$station_id), function(zone) {
  temp.melt = melt(zone[,-1], id.vars=c(1:3, 28:32))
  temp.melt$variable = as.numeric(temp.melt$variable)
  names(temp.melt)[9] = "hour"
  names(temp.melt)[10] = paste("temp", zone[1,1], sep="")
  return (temp.melt)
})
temp.mts = Reduce(function(...) merge(...,all=T), temp.zones)

#Generate timestamps

energy.timestamp = apply(energy.mts, 1, function(i) {
   year = i[1]
   month = if(as.numeric(i[2]) >= 10) i[2] else paste("0", as.numeric(i[2]), sep="")
   day = if(as.numeric(i[3]) >= 10) i[3] else paste("0", as.numeric(i[3]), sep="")
   hour = if((as.numeric(i[9])-1) >= 10) (as.numeric(i[9])-1) else paste("0", (as.numeric(i[9])-1), sep="")

   paste(paste(year, month, day, sep='-'), paste(hour, "00", "00", sep=":"), sep="T") 
})
energy.mts$timestamp = energy.timestamp

temp.timestamp = apply(temp.mts, 1, function(i) {
   year = i[1]
   month = if(as.numeric(i[2]) >= 10) i[2] else paste("0", as.numeric(i[2]), sep="")
   day = if(as.numeric(i[3]) >= 10) i[3] else paste("0", as.numeric(i[3]), sep="")
   hour = if((as.numeric(i[9])-1) >= 10) (as.numeric(i[9])-1) else paste("0", (as.numeric(i[9])-1), sep="")

   paste(paste(year, month, day, sep='-'), paste(hour, "00", "00", sep=":"), sep="T") 
})
temp.mts$timestamp = temp.timestamp

energy.mts[,10:29] = apply(energy.mts[,10:29], 2, function(j) {
    na.indices = which(is.na(j))
    j[na.indices] = -1
    j
})
temp.mts[,10:20] = apply(temp.mts[,10:20], 2, function(j) {
    na.indices = which(is.na(j))
    j[na.indices] = -1
    j
})

energytemp = merge(energy.mts, temp.mts, all=T)


write.csv(energytemp, file="Load_history_mmx.csv", row.names = F, quote=F)
