positions   <- complex(0) # Initialize the history of positions
currentPos  <- 0.0+0.0i   # Start at the origin
NUMBERSTEPS <- 50         # Number of steps to take
angleFacing <- 0.0        # direction it is facing
stdDev      <- 1.0        # std dev. of the change in the angle

step <- as.integer(0)
repeat
    {
        ## Add new people to the line and update the length
        newAngle     <- angle + rnorm(1,0.0,stdDev)
        proposedStep <- currentPos + exp(newAngle*1.0i)
        if(Re(proposedStep) < 0.0)
           next   # Ignore this step. It moves to neg. real parts

        ## update the position
        angle      <- newAngle
        currentPos <- proposedStep
        positions  <- c(positions,currentPos)

        ## Update the current time step
        step <- step + as.integer(1)

        ## Check to see if it is time to stop
        if(step>MAXNUMBER)
            break
    }
plot(Re(positions),Im(positions),type="l")
