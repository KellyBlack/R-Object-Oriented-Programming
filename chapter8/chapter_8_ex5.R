simulateGeometric.Die <- function(theObject,returnResults=FALSE)
    {
        cat("Performing the simulations on the die\n")
        # First reset the trials and start the simulation
        theObject <- resetTrial(theObject)
        repeat
            {
                # Roll a die and get a value.
                thisRoll <- 1+trunc(runif(1,0,6));

                # Add this to the end of the set of results as a
                # character string..
                theObject$trials = append(theObject$trials,as.character(thisRoll))

                if(thisRoll <= 1)
                    {
                        # They rolled a one.  Time to stop!
                        break
                    }
                
            }
        
        if(returnResults) {
            return(as.factor(theObject$trials))
        } else {
            return(theObject)
        }
        
    }
