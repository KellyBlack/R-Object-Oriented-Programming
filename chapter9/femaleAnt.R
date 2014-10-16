# Define the female ant class.
FemaleAnt <- setClass(
    # Set the name of the class
    "FemaleAnt",

    # Name the data types (slots) that the class will track
    slots = c(
        Food ="numeric"     # The number of food units carried
        ),

    # Set the default values for the slots. (optional)
    prototype=list(
        Food=0
        ),

    # Make a function that can test to see if the data is consistent.
    # (optional)
    # This is not called if you have an initialize function defined!
    validity=function(object)
    {
        print("Validity: FemaleAnt")
        # Check to see if the number of offspring is non-negative.
        if(object@Food<0) {
            return("Error: The number of food units is negative")
        }
        return(TRUE)
    },

    # This class inherits from the Ant class
    contains=c("Ant","VIRTUAL")
    )

