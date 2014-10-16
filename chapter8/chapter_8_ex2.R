reset <- function(theObject)
    {
        UseMethod("reset",theObject)
        print("Reset the Trials")
    }

reset.default <- function(theObject)
    {
        print("Uh oh, not sure what to do here!\n")
        return(theObject)
    }

reset.Die <- function(theObject)
    {
        theObject$trials <- character(0)
        print("Reset the die\n")
        return(theObject)
    }

reset.Coin <- function(theObject)
    {
        theObject$trials <- character(0)
        print("Reset the coin\n")
        return(theObject)
    }
