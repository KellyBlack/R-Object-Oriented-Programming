setMethod(f="initialize",
          signature="Ant",
          def=function(.Object,Length=4,Position=c(0.0,0.0,0.0))
          {
              print("Ant initialize")
              .Object = SetLength(.Object,Length)
              .Object = SetPosition(.Object,Position)
              #validObject(.Object)  # you must explicitly call the inspector
              return(.Object)
          }
          )


setMethod(f="initialize",
          signature="FemaleAnt",
          def=function(.Object,Length=4,Position=c(0.0,0.0,0.0))
          {
              print("FemaleAnt initialize ")
              .Object <- callNextMethod(.Object,Length,Position)
              #validObject(.Object)  # you must explicitly call the inspector
              return(.Object)
          }
          )

setMethod(f="initialize",
          signature="WorkerAnt",
          def=function(.Object,Length=4,Position=c(0.0,0.0,0.0))
          {
              print("WorkerAnt initialize")
              .Object <- callNextMethod(.Object,Length,Position)
              #validObject(.Object)  # you must explicitly call the inspector
              return(.Object)
          }
          )

