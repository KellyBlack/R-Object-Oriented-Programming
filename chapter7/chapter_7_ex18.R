updatePosition <- function(currentPos,angle=0.0)
{
    if(abs(angle) > 2.0*pi)
        {
            stop("I arbitrarily do not like angles that big")
        }
    angle <- angle + runif(1,0.0,2.0*pi)
    currentPos + exp(1i*angle)
}

pos1 <- updatePosition(1+2i)
pos2 <- updatePosition(1+2i,3.0*pi)
pos2



                      
