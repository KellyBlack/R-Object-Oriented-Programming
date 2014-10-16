updatePosition <- function(currentPos,angle=0.0)
{
    print(noquote(paste("Angle is ",angle)))
    angle <- angle + runif(1,0.0,2.0*pi)
    currentPos + exp(1i*angle)
}

updatePosition(1+2i)
updatePosition(1+2i,pi)
