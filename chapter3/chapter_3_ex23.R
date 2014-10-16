fileConnector = file("twoBinaryValues.dat",open="wb")
theNumber = as.double(2.72)
writeBin(theNumber,fileConnector,size=4)
note <- "hello there!"
nchar(note)
writeBin(note,fileConnector,size=nchar(note))
close(fileConnector)
