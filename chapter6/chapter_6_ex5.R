fileInfo <- data.frame(time=c("2014-01-01 00:00:00",
                           "2013-12-31 23:59:50",
                           "2013-12-31 23:55:12"),
                       happiness=c(1.0,0.9,0.8))
fileInfo
fileInfo$internalTime <- strptime(fileInfo$time,"%Y-%m-%d %H:%M:%S")
fileInfo
summary(fileInfo)
