aTime <- c("2014-05-05 08:00:00","2014/05/05 08:00:00")
internal <- strptime(aTime,"%Y/%m/%d %H:%M:%S")
internal
is.na(internal)
