# Define the worker  ant class.
WorkerAnt <- setClass(
    # Set the name of the class
    "WorkerAnt",

    # Name the data types (slots) that the class will track
    slots = c(
        Foraging ="logical",    # Whether or not the ant is actively
                                # looking for food

        Alarm = "logical"       # Whether or not the ant is actively
                                # announcing an alarm.
        
        ),

    # Set the default values for the slots. (optional)
    prototype=list(
        Foraging = FALSE,
        Alarm    = FALSE
        ),

    # Make a function that can test to see if the data is consistent.
    # (optional)
    # This is not called if you have an initialize function defined!
    validity=function(object)
    {
        print("Validity: WorkerAnt")
        return(TRUE)
    },

    # This class inherits from the FemaleAnt class
    contains="FemaleAnt"
    )

