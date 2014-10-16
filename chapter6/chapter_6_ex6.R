theTime <- c("08:30:00 1867-07-01","18:15:00 1864-10-27")
converted <- strptime(theTime,"%H:%M:%S %Y-%m-%d",tz="Canada/Eastern")
converted
typeof(converted)
backAgain <- strftime(converted,"%j - %B")
backAgain
typeof(backAgain[1])
