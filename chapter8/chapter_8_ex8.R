# First we create the constructor for the base class
GeometricTrial <- function()
    {
        # Create the basic data structure - a list that keeps track of
        # a set of trials. 
        
        # Create the basic methods as part of a list to be returned.
        me = list(            
            # Define the history to keep track of the trials.
            history = character(0)
            )

        # Define my class identifier and return the list.
        class(me) <- append(class(me),"GeometricTrial")
        return(me)
    }


# Next create the constructor for the Die class
Die <- function()
{
    # Define the object by first calling the constructor for the base class
    me <- GeometricTrial()
    
    # Add the class name to the end of the list of class names
    class(me) <- append(class(me),"Die")
    return(me)
}



# Next create the constructor for the Coin class
Coin <- function()
{
    # Define the object by calling the constructor for the base class
    me <- GeometricTrial()

    # Add the class name to the end of the list of class names
    class(me) <- append(class(me),"Coin")
    return(me)
}




## Define a method to return a copy of the history
getHistory <- function(theObject)
    {
        UseMethod("getHistory",theObject)
    }

getHistory.default <- function(theObject)
    {
        return(factor()) # Just return an empty vector of factors
    }

getHistory.GeometricTrial <- function(theObject)
    {
        return(as.factor(theObject$history))
    }


## Define a method to reset the history
reset <- function(theObject)
    {
        UseMethod("reset",theObject)
    }

reset.default <- function(theObject)
    {
        cat("Uh oh, not sure what to do here!\n")
        return(theObject) # Just return a copy of the object
    }

reset.GeometricTrial <- function(theObject)
    {
        theObject$history <- character(0)
        return(theObject)
    }


## Create the default SingleTrial function that will be
## overridden by child classes.
singleTrial = function(theObject)
    {
        UseMethod("singleTrial",theObject)
    }

singleTrial.default = function(theObject)
    {
        ## Just generate a default success
        warning("Unrecognized object found for the singleTrial method")
        return(list(result="1",success=TRUE))
    }

singleTrial.GeometricTrial = function(theObject)
    {
        NextMethod("singleTrial",theObject)
    }

singleTrial.Coin = function(theObject)
    {
        ## Perform a single coin flip
        value <- as.character(
            cut(as.integer(1+trunc(runif(1,0,2))),c(0,1,2),labels=c("H","T")))
        return(list(result=value,success=(value=="H")))
    }

singleTrial.Die = function(theObject)
    {
        ## Perform a single die roll
        value <- as.integer(1+trunc(runif(1,0,6)))
        return(list(result=value,success=(value==1)))
    }


## Create the default appendEvent
appendEvent = function(theObject,event)
    {
        UseMethod("appendEvent",theObject)
    }

appendEvent.default = function(theObject,event)
    {
        return(theObject) # just return the object.
    }

appendEvent.GeometricTrial = function(theObject,event)
    {
        ## Add the event to the history vector
        theObject$history <- append(theObject$history,event)
        return(theObject)
    }


simulation <- function(theObject)
    {
        UseMethod("simulation",theObject)
    }

simulation.default <- function(theObject)
    {
        warning("Default simulation method called on unrecognized object.")
        return(theObject)
    }

## Define a method to run a simulation of a geometric trial.
simulation.GeometricTrial = function(theObject)
    {
        theObject <- reset(theObject)  # Reset the history
                                        # before the trial.
        repeat
            {
                ## perform a single trial and add it to the history
                thisTrial  <- singleTrial(theObject)
                theObject <- appendEvent(theObject,thisTrial$result)
                if(thisTrial$success)
                    {
                        break  # The trial resulted in a success. Time
                               # to stop!
                    }
            } # The trial was not a success. Keep going.

        return(theObject)

    }
