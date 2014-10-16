theTime <- c("08:30:00 1867-07-01","18:15:00 1864-10-27")
converted <- strptime(theTime,"%H:%M:%S %Y-%m-%d",tz="Canada/Eastern")
converted
converted[1]-converted[2]
