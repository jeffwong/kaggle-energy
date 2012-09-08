require(plyr)

##################
#Utility functions
##################

#Remove the NA rows meant for forecasting from the load data
input.partition = function(rawLoadData) {
    ddply(rawLoadData, .variables = "zone_id", .fun = function(zone) {
        zone[1:1643,]
    })
}

#Transform raw data into fastVAR format
transform.fastVAR = function(rawLoadData, rawTempData) {
    load = data.frame(dlply(rawLoadData, .variables="zone_id", .fun = function(zone) {
        as.vector(t(zone[,-(1:4)]))
    }))
    temp = data.frame(dlply(rawTempData, .variables="station_id", .fun = function(station) {
        as.vector(t(station[,-(1:4)]))
    }))
    return (list(load = as.matrix(load), temp = as.matrix(temp)))
}
