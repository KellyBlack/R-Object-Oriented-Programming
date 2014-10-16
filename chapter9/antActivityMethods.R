setGeneric(name="GetLength",
           def=function(antie)
           {
               standardGeneric("GetLength")
           }
           )

setMethod(f="GetLength",
          signature="Ant",
          definition=function(antie)
          {
              return(antie@Length)
          }
          )



setGeneric(name="SetLength",
           def=function(antie,newLength)
           {
               standardGeneric("SetLength")
           }
           )

setMethod(f="SetLength",
          signature="Ant",
          definition=function(antie,newLength)
          {
              if(newLength>0.0) {
                  antie@Length = newLength
              } else {
                  warning("Error - invalid length passed");
              }

              return(antie)
          }
          )


setGeneric(name="SetPosition",
           def=function(antie,newPosition)
           {
               standardGeneric("SetPosition")
           }
           )

setMethod(f="SetPosition",
          signature="Ant",
          definition=function(antie,newPosition)
          {
              antie@Position = newPosition
              return(antie)
          }
          )


setGeneric(name="SetActivityLevel",
           def=function(antie,activity)
           {
               standardGeneric("SetActivityLevel")
           }
           )

setMethod(f="SetActivityLevel",
          signature=c("Ant","logical"),
          definition=function(antie,activity)
          {
              if(activity) {
                  antie@ActivityLevel = 0.1
              } else {
                  antie@ActivityLevel = 0.0
              }
              return(antie)
          }
          )

setMethod(f="SetActivityLevel",
          signature=c("Ant","numeric"),
          definition=function(antie,activity)
          {
              if(activity>=0.0) {
                  antie@ActivityLevel = activity
              } else {
                  warning("The activity level cannot be negative")
              }
              return(antie)
          }
          )

