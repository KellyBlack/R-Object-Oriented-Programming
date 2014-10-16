fileConnector = file("twoBinaryValues.dat",open="rb")
value <- readBin(fileConnector,double(),1,size=4)
value
note <- readBin(fileConnector,character(),12,size=1)
note
close(fileConnector)
