require(plyr)

##################
#Utility functions
##################

#Transform raw data into fastVAR format
transform.fastVAR = function(rawLoadData, rawTempData) {
    load = data.frame(dlply(rawLoadData, .variables="zone_id", .fun = function(zone) {
        as.vector(t(zone[,-(1:4)]))
    }))
    temp = data.frame(dlply(rawTempData, .variables="station_id", .fun = function(station) {
        as.vector(t(station[,-(1:4)]))
    }))
    return (list(load, temp))
}
