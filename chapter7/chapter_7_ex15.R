updatePosition <- function(currentPos,angle,stdDev)
{
    angle <- angle + rnorm(1,0.0,stdDev)
    currentPos + exp(1i*angle)
}

angle <- 0.0
updatePosition(1+2i,angle,1.0)
angle
