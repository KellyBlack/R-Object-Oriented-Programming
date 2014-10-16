updatePosition <- function(currentPos)
{
    newDirection <- exp(1i*runif(1,0.0,2.0*pi))
    currentPos + newDirection
}

updatePosition(0.0)
updatePosition
