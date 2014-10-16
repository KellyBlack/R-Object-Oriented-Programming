theList <- list(one=c(1,2,3),two=c(TRUE,FALSE,TRUE,TRUE))
sumResult <- lapply(theList,sum)
sumResult
typeof(sumResult)
sumResult$one
