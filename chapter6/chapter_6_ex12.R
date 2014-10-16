later <- strptime("2014-01-01 12:00:00","%Y-%m-%d %H:%M:%S")
oneHour <- as.difftime(1,units="hours")
later+oneHour
