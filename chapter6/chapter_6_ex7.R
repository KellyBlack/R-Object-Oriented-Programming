earlier <- strptime("2014-01-01 00:00:00","%Y-%m-%d %H:%M:%S")
later <- strptime("2014-01-02 00:00:00","%Y-%m-%d %H:%M:%S")
later-earlier
timeDiff <- later-earlier
timeDiff
as.double(timeDiff)
earlier+timeDiff
