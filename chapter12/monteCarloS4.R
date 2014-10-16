

######################################################################
# Create the Monte Carlo class
#
# This class is used to make many simulations
MonteCarlo <- setClass(
    # Set the name for the class
    "MonteCarlo",

    # Define the slots
    slots = c(

      # First define the parameters for the stochastic model
      N        = "numeric",
      T        = "numeric",
      x0       = "numeric",
      y0       = "numeric",
      alpha    = "numeric",
      beta     = "numeric",
      gamma    = "numeric",
      delta    = "numeric",
      noiseOne = "numeric",
      noiseTwo = "numeric",

      # Define the data to track and the number of trials
      xData = "numeric",
      yData = "numeric"
    
        ),

    # Set the default values for the slots. (optional)
    prototype=list(
      xData = double(0),
      yData = double(0)
        ),

    # Make a function that can test to see if the data is consistent.
    # This is not called if you have an initialize function defined!
    validity=function(object)
    {
      return(TRUE)
    }
    )



## the methods to set the parameters
setGeneric(name="setParams",
           def=function(monteCarlo,N,T,x0,y0,alpha,beta,gamma,delta,noiseOne,noiseTwo)
           {
               standardGeneric("setParams")
           }
           )

setMethod(f="setParams",
          signature="MonteCarlo",
          definition=function(monteCarlo,N,T,x0,y0,alpha,beta,gamma,delta,noiseOne,noiseTwo)
          {
            # Set the values of all of the parameters
            monteCarlo@N <- N
            monteCarlo@T <- T
            monteCarlo@x0 <- x0
            monteCarlo@y0 <- y0
            monteCarlo@alpha <- alpha
            monteCarlo@beta <- beta
            monteCarlo@gamma <- gamma
            monteCarlo@delta <- delta
            monteCarlo@noiseOne <- noiseOne
            monteCarlo@noiseTwo <- noiseTwo
            return(monteCarlo)
          }
          )

## the methods to get the parameters
setGeneric(name="getParams",
           def=function(monteCarlo)
           {
               standardGeneric("getParams")
           }
           )

setMethod(f="getParams",
          signature="MonteCarlo",
          definition=function(monteCarlo)
          {
            # return the values of all of the parameters
            return(c(monteCarlo@N,
                     monteCarlo@T,
                     monteCarlo@x0,
                     monteCarlo@y0,
                     monteCarlo@alpha,
                     monteCarlo@beta,
                     monteCarlo@gamma,
                     monteCarlo@delta,
                     monteCarlo@noiseOne,
                     monteCarlo@noiseTwo))
          }
          )

## the methods to do prepare for the simulations.
setGeneric(name="prepare",
           def=function(monteCarlo,number)
           {
               standardGeneric("prepare")
           }
           )

setMethod(f="prepare",
          signature="MonteCarlo",
          definition=function(monteCarlo,number)
          {
            # Set the number of trials and initialize the values
            monteCarlo@xData <- double(number)
            monteCarlo@yData <- double(number)
            return(monteCarlo)
          }
          )

## the methods to set the value for a set of simulations
setGeneric(name="setValue",
           def=function(monteCarlo,x,y,i)
           {
               standardGeneric("setValue")
           }
           )

setMethod(f="setValue",
          signature="MonteCarlo",
          definition=function(monteCarlo,x,y,i)
          {
            # Set the number of trials and initialize the values
            monteCarlo@xData[i] <- x
            monteCarlo@yData[i] <- y
            return(monteCarlo)
          }
          )


## the methods to get the value for a set of simulations
setGeneric(name="getValues",
           def=function(monteCarlo)
           {
               standardGeneric("getValues")
           }
           )

setMethod(f="getValues",
          signature="MonteCarlo",
          definition=function(monteCarlo)
          {
            # Set the number of trials and initialize the values
            return(matrix(c(monteCarlo@xData,monteCarlo@yData),ncol=2))
          }
          )

## the methods to run the  simulations.
setGeneric(name="simulations",
           def=function(monteCarlo,number,simulation)
           {
               standardGeneric("simulations")
           }
           )

setMethod(f="simulations",
          signature="MonteCarlo",
          definition=function(monteCarlo,number,simulation)
          {
            # Set the number of trials and initialize the values
            monteCarlo <- prepare(monteCarlo,number)
            params <- getParams(monteCarlo)   # get the parameters

            # Perform the simulations
            lupe <- 0
            while(lupe < number)
              {
                lupe <- lupe + 1 # increment the count
                simulation <- singleSimulation(
                      simulation,
                      params[1],params[2],params[3],params[4],params[5],
                      params[6],params[7],params[8],params[9],params[10])

                values <- getFinalValues(simulation)
                monteCarlo <- setValue(monteCarlo,values[1],values[2],lupe)
              }

            return(monteCarlo)
          }
          )




# the methods to plot the results
setMethod(f="hist",
          signature="MonteCarlo",
          definition=function(x,...)
          {
            plotWindows <- dev.list()
            while(length(plotWindows)<2)
              {
                dev.new()
                plotWindows <- dev.list()
              }
            
            values <- getValues(x)
            isValid <- (!is.na(values[,1])) && (!is.infinite(values[,1]))
            
            dev.set(plotWindows[1])
            hist(values[isValid,1],...)

            dev.set(plotWindows[2])
            hist(values[isValid,2],...)

            
          }
          )

