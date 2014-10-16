updatePosition <- function(currentPos,angle,stdDev)
{
    angle <- angle + rnorm(1,0,stdDev)
    list(newPos=currentPos + exp(angle*1.0i),
         newAngle=angle)
}

pos <- updatePosition(2.0,0.0,1.0)
pos
pos$newPos
