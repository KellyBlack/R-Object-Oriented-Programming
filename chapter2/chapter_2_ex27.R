d <- data.frame(one=c(1,2,3),two=as.factor(c("one","two","three")))
e <- data.frame(one=c(4,5,6),two=as.factor(c("vier","funf","sechs")))
newDataFrame <- rbind(d,e)
newDataFrame
