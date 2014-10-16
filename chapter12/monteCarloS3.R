

######################################################################
# Create the Monte Carlo class
#
# This class is used to make many simulations
MonteCarlo <- function()
{

    # Define the slots
    me = list(

        ## First define the parameters for the stochastic model
        N        = 0,
        T        = 0,
        x0       = 0,
        y0       = 0,
        alpha    = 0,
        beta     = 0,
        gamma    = 0,
        delta    = 0,
        noiseOne = 0,
        noiseTwo = 0,

        ## Define the data to track and the number of trials
        xData = 0,
        yData = 0


      )


  ## Set the name for the class
  class(me) <- append(class(me),"MonteCarlo")
  return(me)
}


## Define the function used to perform the Monte Carlo simulations.
getParams <- function(monteCarlo)
  {
    UseMethod("getParams",monteCarlo)
  }

getParams.default <- function(monteCarlo)
    {
      print("getParams.default not defined!")
      return(NA)
    }


## define the function to get the parameters as a vector
getParams.MonteCarlo <- function(monteCarlo)
  {
      ## return the values of all of the parameters
      return(c(monteCarlo$N,monteCarlo$T,monteCarlo$x0,monteCarlo$y0,
               monteCarlo$alpha,monteCarlo$beta,monteCarlo$gamma,monteCarlo$delta,
               monteCarlo$noiseOne,monteCarlo$noiseTwo))
  }

setParams <- function(monteCarlo,N,T,x0,y0,alpha,beta,gamma,delta,noiseOne,noiseTwo)
    {
        UseMethod("setParams",monteCarlo)
    }

setParams.default <- function(monteCarlo,N,T,x0,y0,alpha,beta,gamma,delta,noiseOne,noiseTwo)
    {
        ## Do not know what to do here, so do not make any changes.
        return(monteCarlo)
    }

# Define the method to set all of the parameters to use in a
# simulation at once.
setParams.MonteCarlo <- function(
    monteCarlo,
    N,T,x0,y0,alpha,beta,gamma,delta,noiseOne,noiseTwo)
    {
        ## Set the values of all of the parameters
        monteCarlo$N <- N
        monteCarlo$T <- T
        monteCarlo$x0 <- x0
        monteCarlo$y0 <- y0
        monteCarlo$alpha <- alpha
        monteCarlo$beta <- beta
        monteCarlo$gamma <- gamma
        monteCarlo$delta <- delta
        monteCarlo$noiseOne <- noiseOne
        monteCarlo$noiseTwo <- noiseTwo
        return(monteCarlo)
    }

# Define the method used to initialize the data prior to a run.
prepare <- function(monteCarlo,number)
    {
        UseMethod("prepare",monteCarlo)
    }

prepare.default <- function(monteCarlo,number)
    {
        ## Not sure what to do here. So do nuthn!
        return(monteCarlo)
    }

prepare.MonteCarlo <- function(monteCarlo,number)
    {
        ## Set the number of trials and initialize the values to
        ## zeroes.
        monteCarlo$xData <- double(number)
        monteCarlo$yData <- double(number)
        return(monteCarlo)
    }

# Define the method to set the value for a single data pair
setValue <- function(monteCarlo,x,y,i)
    {
        UseMethod("setValue",monteCarlo)
    }

setValue.default <- function(monteCarlo,x,y,i)
    {
        ## Not sure what to do so do nuthing
        return(monteCarlo)
    }

setValue.MonteCarlo <- function(monteCarlo,x,y,i)
    {
        ## Set the number of trials and initialize the values
        monteCarlo$xData[i] <- x
        monteCarlo$yData[i] <- y
        return(monteCarlo)
    }


## Define the method to get all of the data as a matrix
getValues <- function(monteCarlo)
    {
        UseMethod("getValues",monteCarlo)
    }

getValues.default <- function(monteCarlo)
    {
        ## Not sure what to do. Return NA
        return(NA)
    }

getValues.MonteCarlo <- function(monteCarlo)
    {
        ## Set the number of trials and initialize the values
        return(matrix(c(monteCarlo$xData,monteCarlo$yData),ncol=2))
    }



## Define the function used to perform the Monte Carlo simulations.
simulations <- function(monteCarlo,number,simulation)
  {
    UseMethod("simulations",monteCarlo)
  }

simulations.default <- function(monteCarlo,number,simulation)
    {
      print("simulations.default not defined!")
      return(simulation)
    }


simulations.MonteCarlo <- function(monteCarlo,number,simulation)
  {
    ## Set the number of trials and initialize the values
    monteCarlo <- prepare(monteCarlo,number)
    params <- getParams(monteCarlo)   # get the parameters

    ## Perform the simulations
    lupe <- 0
    while(lupe < number)
      {
        lupe <- lupe + 1 # increment the count
        ## Perform a single simulation.
        simulation <- singleSimulation(
          simulation,
          params[1],params[2],params[3],params[4],params[5],
          params[6],params[7],params[8],params[9],params[10])

        ## Get the last values of the simulation and record them.
        values <- getFinalValues(simulation)
        monteCarlo <- setValue(monteCarlo,values[1],values[2],lupe)
      }

    return(monteCarlo)
  }




# the methods to plot the results
hist.MonteCarlo <- function(x,main="",...)
  {
      par(mfrow=c(2,1))
      values <- getValues(x)
      isValid <- (!is.na(values[,1])) && (!is.infinite(values[,1]))
      hist(values[isValid,1],xlab="x",main=main,...)
      hist(values[isValid,2],xlab="y",main="",...)
            
  }


