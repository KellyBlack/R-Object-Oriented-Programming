

######################################################################
# Create the Monte Carlo class
#
# This class is used to make many simulations
MonteCarlo <- function()
{
  ## Get the environment for this
  ## instance of the function.
  thisEnv <- environment()


  ## First define the parameters for the stochastic model
  N        <- 0
  T        <- 0
  x0       <- 0
  y0       <- 0
  alpha    <- 0
  beta     <- 0
  gamma    <- 0
  delta    <- 0
  noiseOne <- 0
  noiseTwo <- 0

  ## Define the data to track and the number of trials
  xData <- 0
  yData <- 0

    # Define the slots
    me = list(

      ## Define the environment where this list is defined so
      ## that I can refer to it later.
      thisEnv = thisEnv,

      getParams = function()
      {
        ## return the values of all of the parameters
        return(c(N,T,x0,y0,
                 alpha,beta,gamma,delta,
                 noiseOne,noiseTwo))
      },

      setParams = function(
        N,T,x0,y0,alpha,beta,gamma,delta,noiseOne,noiseTwo)
      {
        ## Set the values of all of the parameters
        N <<- N
        T <<- T
        x0 <<- x0
        y0 <<- y0
        alpha <<- alpha
        beta <<- beta
        gamma <<- gamma
        delta <<- delta
        noiseOne <<- noiseOne
        noiseTwo <<- noiseTwo
      },

      prepare = function(number)
      {
        ## Set the number of trials and initialize the values
        xData <<- double(number)
        yData <<- double(number)
      },

      setValue = function(x,y,i)
      {
        ## Set the number of trials and initialize the values
        xData[i] <<- x
        yData[i] <<- y
      },

      
      getValues = function()
      {
        ## Set the number of trials and initialize the values
        return(matrix(c(xData,yData),ncol=2))
      }

      )


  ## Define the value of the list within the current environment.
  assign('this',me,envir=thisEnv)


  ## Set the name for the class
  class(me) <- append(class(me),"MonteCarlo")
  return(me)
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
    monteCarlo$prepare(number)
    params <- monteCarlo$getParams()   # get the parameters

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
        values <- simulation$getFinalValues()
        monteCarlo$setValue(values[1],values[2],lupe)
      }

    return(monteCarlo)
  }




# the methods to plot the results
hist.MonteCarlo <- function(x,main="",...)
  {
      par(mfrow=c(2,1))
      values <- x$getValues()
      isValid <- (!is.na(values[,1])) && (!is.infinite(values[,1]))
      hist(values[isValid,1],xlab="x",main=main,...)
      hist(values[isValid,2],xlab="y",main="",...)
            
  }


