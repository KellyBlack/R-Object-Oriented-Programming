# Define the base coordinates class.
Coordinate <- setClass(
    # Set the name of the class
    "Coordinate",

    # Name the data types (slots) that the class will track
    slots = c(
        x="numeric",  # the x position
        y="numeric"   # the y position
        ),

    # Set the default values for the slots. (optional)
    prototype=list(
        x=0.0,
        y=0.0
        ),

    # Make a function that can test to see if the data is consistent.
    # (optional)
    # This is not called if you have an initialize function defined!
    validity=function(object)
    {
        # Check to see if the coordinate is outside of a circle of radius 100
        print("Checking the validity of the point")
        if(object@x*object@x+object@y*object@y>100.0*100.0) {
            return("Error: The point is too far away from the origin.")
        } 
        return(TRUE)
    }
    )

# Add a method to set the value of a coordinate
setGeneric(name="SetPoint",
           def=function(coord,x,y)
           {
               standardGeneric("SetPoint")
           }
           )

setMethod(f="SetPoint",
          signature="Coordinate",
          def=function(coord,x,y)
          {
              print("Setting the point")
              coord@x = x
              coord@y = y
              return(coord)
          }
          )



setMethod(f="show",
          signature="Coordinate",
          def=function(object)
          {
              cat("The coordinate is X: ",object@x," Y: ",object@y,"\n")
          }
          )


setMethod(f="initialize",
          signature="Coordinate",
          def=function(.Object,x=0.0,y=0.0)
          {
              print("Checking the point")
              .Object = SetPoint(.Object,x,y)
              validObject(.Object)  # you must explicitly call the inspector
              return(.Object)
          }
          )


