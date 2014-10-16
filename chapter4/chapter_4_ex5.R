# Generate 500 random numbers with a chi-squared dist. with 20 df.
degrees <- 20
x <- rchisq(500,df=degrees)

# Get the 95th and 99.95th percentile
percent95   <- qchisq(0.95,df=degrees)
percent9995 <- qchisq(0.9995,df=degrees)

# Plot a histogram of the distribution
theTitle <- bquote("Random Numbers That Follow a "~chi^2~" distribution(df"==.(format(degrees))~")")
histInfo <-
    hist(x,
         main=theTitle,
         xlab="Number",ylab="Proportion",freq=FALSE,ylim=c(0,0.07),xlim=c(0,percent9995))

# Plot the true density function
domain <- seq(0,percent9995,by=0.02)
points(domain,dchisq(domain,df=degrees),type='l',col=1,lty=2)

# Plot an approximation of the density function.
densityApprox <- density(x)
points(densityApprox,type='l',col=1,lty=3)

# Add a legend
legend(percent9995-20,0.07,c("True Density","Approximate Density"),lty=c(2,3))

# Add an area to the right of the 95th percentile.
xRight  <- c(percent95,seq(percent95,percent9995),percent9995,percent95)
theArea <- c(0,dchisq(seq(percent95,percent9995),df=degrees),0,0)
polygon(xRight,theArea, col=rgb(0.3,0.3,0.3,0.5))
 
