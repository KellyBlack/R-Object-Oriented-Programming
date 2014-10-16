theTime <- c("08:30:00 1867-07-01","18:15:00 1864-10-27")
converted <- strptime(theTime,"%H:%M:%S %Y-%m-%d",tz="Canada/Eastern")
converted
typeof(converted[1])
otherTime <- as.POSIXct(converted)
otherTime
typeof(otherTime[1])
cat(otherTime[1],"\n")
