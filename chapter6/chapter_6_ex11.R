earlier <- strptime("2014-01-01 00:00:00","%Y-%m-%d %H:%M:%S")
later <- strptime("2014-01-01 12:00:00","%Y-%m-%d %H:%M:%S")
timeDiff <- difftime(later,earlier,units="sec")
timeDiff
timeDiff <- difftime(later,earlier,units="day")
timeDiff
