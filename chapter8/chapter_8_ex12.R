# First we create the constructor for the base class
GeometricTrial <- function()
    {
        # Create the basic data structure - a zero length vector of
        # character objects. Also, get the environment used within
        # this function.
        history <- character(0)
        thisEnv <- environment()
        
        # Create the basic methods as part of a list to be returned.
        me = list(
            
            # Define the environment where this list is defined so
            # that I can refer to it later.
            thisEnv = thisEnv,

            # Define a method to zero out the history vector.  The
            # history vector lives in the parent environment so use
            # <<- as opposed to <-
            reset = function()
            {
                history <<- character(0)
            },

            # Define a method to return a copy of the history
            getHistory = function()
            {
                return(as.factor(history))
            },

            # Define a method to run a simulation of a geometric trial.
            simulation = function()
            {

                # Get my single trial function. First get the
                # environment in which my data is saved. Then get a
                # copy of the SingleTrial method that is saved in that
                # environment.
                this <- get("this",thisEnv)
                singleTrial <- get("SingleTrial",thisEnv)
                
                # Now reset the trials and start the simulation
                me$reset()
                repeat
                    {
                        # perform a single trial and add it to the history
                        thisTrial  <- singleTrial()
                        history   <<- append(history,thisTrial$result)
                        if(thisTrial$success)
                            {
                                # The trial resulted in a success. Time to stop!
                                break
                            }
                        # The trial was not a success. Keep going.
                    }

                return(as.factor(history))

            }
            )

        # Define the value of the list within the current environment.
        assign('this',me,envir=thisEnv)

        # Create the default SingleTrial function that will be overridden by child classes.
        assign("SingleTrial",function()
            {
                # Just generate a default success so that it always
                # stop at the first trial.
                return(list(result="1",success=TRUE))
            },thisEnv)


        # Define my class identifier and return the list.
        class(me) <- "GeometricTrial"
        return(me)
    }


# Next create the constructor for the Die class
Die <- function()
{
    # Define the object by first calling the constructor for the base class
    me <- GeometricTrial()

    # Save a copy of the single trial function in the environment
    # defined by the base class
    assign("SingleTrial",function()
           {
               # Generate a number between 0 and 6 with a uniform prob. density
               result <- 1+trunc(runif(1,0,6));
               if(result<=1)
                   {
                       # This is a one.
                       return(list(result="1",success=TRUE))
                   }
               # Otherwise return the value of the roll
               return(list(result=as.character(result),success=FALSE))
           },me$thisEnv)

    # Add the class name to the end of the list of class names
    class(me) <- append(class(me),"Coin")
    return(me)
}



# Next create the constructor for the Coin class
Coin <- function()
{
    # Define the object by calling the constructor for the base class
    me <- GeometricTrial()

    # Save a copy of the single trial function in the environment
    # defined by the base class
    assign("SingleTrial",function()
        {
            # Generate a number between 0 and 2 with a uniform prob. density
            result <- runif(1,0,2);
            if(result<1)
                {
                    # This is a heads.
                    return(list(result="H",success=TRUE))
               }
            # Otherwise assume that it is a tails
            return(list(result="T",success=FALSE))
        },me$thisEnv)

    # Add the class name to the end of the list of class names
    class(me) <- append(class(me),"Die")
    return(me)
}
