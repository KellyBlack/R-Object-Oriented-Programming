positions   <- complex(0) # Initialize the history of positions
currentPos  <- 0.0+0.0i   # Start at the origin
NUMBERSTEPS <- 50         # Number of steps to take
angleFacing <- 0.0        # direction it is facing
stdDev      <- 1.0        # std dev. of the change in the angle

step <- as.integer(0)
repeat
    {
        ## Update the current time step
        step <- step + as.integer(1)

        ## Check to see if it is time to stop
        if(step>MAXNUMBER)
            break

        ## Add new people to the line and update the length
        angle <- angle + rnorm(1,0.0,stdDev)
        currentPos <- currentPos + exp(angle*1.0i)
        positions  <- c(positions,currentPos)
        
    }

plot(Re(positions),Im(positions),type="l")
