updatePosition <- function(currentPos,angle,stdDev)
{
    angle <- angle + rnorm(1,0.0,stdDev)
    newStep <- exp(angle*1.0i)
    if(Re(currentPos + newStep)<0.0)
        {
            # This would be a move in the left hand part of the plane.
            # Move in the opposite direction.
            return(list(newPos=currentPos - newStep,
                        newAngle=angle+pi))
        }
    # All is good. Accept this move.
    return(list(newPos=currentPos + newStep,
                angle=angle))
}

pos <- updatePosition(-0.1+2i,0.0,1.0)
pos$newPos
