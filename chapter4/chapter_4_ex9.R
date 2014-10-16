x <- rpois(500,lambda=10)
theTitle <- bquote("Random Numbers That Follow a Poisson distribution("~lambda==~"10)")
barplot(table(x),main=theTitle,xlab="Numbers",ylab="Frequency")
