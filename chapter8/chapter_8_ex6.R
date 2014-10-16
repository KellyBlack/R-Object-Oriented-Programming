simulateGeometric.Coin <- function(theObject,returnResults=FALSE)
    {
        cat("Performing the simulations on the coin\n")
        theObject <- resetTrial(theObject)
        # First reset the trials and start the simulation
        repeat
            {
                # Flip a coin and get a value.
                thisFlip <- runif(1,0,2);
                if(thisFlip <= 1)
                    {
                        # They flipped a heads, append this event as a
                        # character to the end and then stop!
                        theObject$trials = append(theObject$trials,"H")
                        break
                    }
                # This was not a one. Just add this to the end of the
                # set of results as a character string..
                theObject$trials = append(theObject$trials,as.character("T"))
            }
        if(returnResults) {
            return(as.factor(theObject$trials))
        } else {
            return(theObject)
        }
        
    }
