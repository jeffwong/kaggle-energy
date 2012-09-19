require(reshape2)
require(timeDate)
require(lubridate)

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

energy$business_day = F
energy$business_day[which(isHoliday(energy.dates, holidays, 1:5))] = T

#Identify Season

energy$season = getSeason(energy.dates)

#Identify day of week

energy$day_of_week = wday(energy.dates, label=T)

#Melt

energy.melt = melt(energy, id.vars=c(1:4, 29:33))
energy.melt$variable = as.numeric(energy.melt$variable)
names(energy.melt)[10] = "hour"
names(energy.melt)[11] = "measurement"

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

temp$business_day = F
temp$business_day[which(isHoliday(temp.dates, holidays, 1:5))] = T

temp$season = getSeason(temp.dates)

temp$day_of_week = wday(temp.dates, label=T)

temp.melt = melt(temp, id.vars=c(1:4, 29:33))
temp.melt$variable = as.numeric(temp.melt$variable)
names(temp.melt)[10] = "hour"
names(temp.melt)[11] = "measurement"

temp.melt$measurement_type = "temperature"
energy.melt$measurement_type = "load"

#Generate timestamps

energy.timestamp = apply(energy.melt, 1, function(i) {
   year = i[2]
   month = if(as.numeric(i[3]) >= 10) i[3] else paste("0", as.numeric(i[3]), sep="")
   day = if(as.numeric(i[4]) >= 10) i[4] else paste("0", as.numeric(i[4]), sep="")
   hour = if((as.numeric(i[10])-1) >= 10) (as.numeric(i[10])-1) else paste("0", (as.numeric(i[10])-1), sep="")

   paste(paste(year, month, day, sep='-'), paste(hour, "00", "00", sep=":"), sep="T") 
})
energy.melt$timestamp = energy.timestamp

temp.timestamp = apply(temp.melt, 1, function(i) {
   year = i[2]
   month = if(as.numeric(i[3]) >= 10) i[3] else paste("0", as.numeric(i[3]), sep="")
   day = if(as.numeric(i[4]) >= 10) i[4] else paste("0", as.numeric(i[4]), sep="")
   hour = if((as.numeric(i[10])-1) >= 10) (as.numeric(i[10])-1) else paste("0", (as.numeric(i[10])-1), sep="")

   paste(paste(year, month, day, sep='-'), paste(hour, "00", "00", sep=":"), sep="T") 
})
temp.melt$timestamp = temp.timestamp

names(temp.melt)[1] = "id"
names(energy.melt)[1] = "id"
energy.melt$id = paste("L", energy.melt$id, sep="")
temp.melt$id = paste("T", temp.melt$id, sep="")

energy.melt = energy.melt[-which(is.na(energy.melt$measurement)),]
temp.melt = temp.melt[-which(is.na(temp.melt$measurement)),]


write.csv(rbind(energy.melt, temp.melt), file="Load_history_mmx.csv", row.names = F, quote=F)
