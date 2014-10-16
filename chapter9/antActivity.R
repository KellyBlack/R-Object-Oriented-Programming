# Define the base Ant class.
Ant <- setClass(
    # Set the name of the class
    "Ant",

    # Name the data types (slots) that the class will track
    slots = c(
        Length="numeric",           # the length (size) of this ant.
        
        Position="numeric",         # the position of this ant. 
                                    # (a 3 vector!)
        
        pA="numeric",               # Probability that an ant will 
                                    # transition from active to 
                                    # inactive.

        pI="numeric",               # Probability that an ant will 
                                    # transition from inactive to 
                                    # active.

        ActivityLevel="numeric"     # The ant's current activity level.

        ),

    # Set the default values for the slots. (optional)
    prototype=list(
        Length=4.0,
        Position=c(0.0,0.0,0.0),
        pA=0.05,
        pI=0.1,
        ActivityLevel=0.5
        ),

    # Make a function that can test to see if the data is consistent.
    # (optional)
    validity=function(object)
    {
        # Check to see if the activity level and length is
        # non-negative.
        # See the discussion on the @ notation in the text below.
        if(object@ActivityLevel<0.0) {
            return("Error: The activity level is negative")
        } else if (object@Length<0.0) {
            return("Error: The length is negative")
        }
        return(TRUE)
    }
    )
