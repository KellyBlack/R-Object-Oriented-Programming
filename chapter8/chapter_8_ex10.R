source('chapter_8_ex8.R')
source('chapter_8_ex9.R')

simulateGeometric.default <- function(theObject,returnResults=FALSE)
    {
        cat("Not sure what to do here?\n")
        return(theObject)
    }


simulateGeometric.GeometricTrial <- function(theObject,returnResults=FALSE)
    {
        cat("This is an object whose base class is a GeometricTrial. ",
            "Need to check the child classes!\n")
        theObject <- resetTrial(theObject)
        NextMethod("simulateGeometric",theObject)
    }

simulateGeometric.Die <- function(theObject,returnResults=FALSE)
    {
        cat("Performing the simulations on the die\n")
        # First reset the trials and start the simulation
        repeat
            {
                # Roll a die and get a value.
                thisRoll <- 1+trunc(runif(1,0,6));

                # Add this to the end of the set of results as a
                # character string..
                theObject$trials = append(theObject$trials,as.character(thisRoll))
                
                if(thisRoll <= 1) {
                    # They rolled a one. Time to stop!
                    break
                }
            }

        # Decide what value to return
        if(returnResults) {
            return(as.factor(theObject$trials))
        } else {
            return(theObject)
        }
        
    }

simulateGeometric.Coin <- function(theObject,returnResults=FALSE)
    {
        cat("Performing the simulations on the coin\n")
        # First reset the trials and start the simulation
        repeat
            {
                # Flip a coin and get a value.
                thisFlip <- runif(1,0,2)

                if(thisFlip <= 1) {
                    # They flipped a heads. Time to stop!
                    theObject$trials = append(theObject$trials,"H")
                    break
                    }
                # The flip is a tails. Add this to the end of the set
                # of results as a character string..
                theObject$trials = append(theObject$trials,"T")

            }
        
        if(returnResults) {
            return(as.factor(theObject$trials))
        } else {
            return(theObject)
        }
        
    }

