

######################################################################
## Create the base simulation class
##
## This is used to represent a single simulation
Simulation <- function()
  {
      
      ## Create the list used to represent an
      ## object for this class
      me = list(
          simulationResults = matrix(0)
          )

      ## Set the name for the class
      class(me) <- append(class(me),"Simulation")
      return(me)


  }

## define the method to get the simulation results
getSimulation <- function(theSimulation)
    {
        UseMethod("getSimulation",theSimulation)
    }

getSimulation.default <- function(theSimulation)
    {
        ## Not sure what to do here. Just punt
        return(NA)
    }

getSimulation <- function(theSimulation)
    {
        ## Return the value of the  variable theSimulation
        return(theSimulation$simulationResults)
    }


# Set the data values that are the result of a simulation.
setSimulation <- function(theSimulation)
    {
        UseMethod("setSimulation",theSimulation)
    }

setSimulation.default <- function(theSimulation)
    {
        # Not sure what to do so do nothing!
        return(theSimulation)
    }

setSimulation <- function(theSimulation,simulationResults)
    {
        ## Set the value of the  variable theSimulation
        theSimulation$simulationResults <- simulationResults
        return(theSimulation)
    }

## method to return the data from the current set of results.
getFinalValues <- function(theSimulation)
    {
        UseMethod("getFinalValues",theSimulation)
    }

getFinalValues <- function(theSimulation)
    {
        ## Not sure what to do so send back a lot of nothing
        return(NA)
    }

getFinalValues <- function(theSimulation)
    {
        ## Get the value of the data pair at the last time step
        size <- dim(theSimulation$simulationResults)
        return(c(theSimulation$simulationResults[size[1],1],
                 theSimulation$simulationResults[size[1],2]))
    }






######################################################################
## Create a simulation for a discrete simulation.
##
## This is used to represent the results from a discrete simulation.
DiscreteSimulation <- function()
  {
    ## Define the base class and get the environment
    me <- Simulation()
    me$N <- 0

    ## Set the name for the class with a numeric grade associated with it.
    class(me) <- append(class(me),"DiscreteSimulation")
    return(me)
  }

## Set the accessors for the number of steps.
getNumber <- function(me)
    {
        UseMethod("getNumber",me)
    }

getNumber.default <- function(me)
    {
        ## Not sure what to do here. Just give up
        return(NA)
    }

getNumber.DiscreteSimulation <- function(me)
    {
        return(me$N)
    }

setNumber <- function(me,number)
    {
        UseMethod("setNumber",me)
    }

setNumber.default <- function(me,number)
    {
        ## NOt sure what to do so do nothing
        return(me)
    }

setNumber.DiscreteSimulation <- function(me,number)
    {
        me$N <- number
        return(me)
    }





######################################################################
## Create a simulation for a continuous simulation.
##
## This is used to represent the results from a continuous simulation.
ContinuousSimulation <- function()
  {
    ## Define the base class and get the environment
    me <- Simulation()
    me$dt <- 0

    ## Set the name for the class with a numeric grade associated with it.
    class(me) <- append(class(me),"ContinuousSimulation")
    return(me)

  }


## Define the accessors for the time step.
getDT <- function(me)
    {
        UseMethod("getDT",me)
    }

getDT.default <- function(me)
    {
        ## Nothing to return...
        return(NA)
    }

getDT.ContinuousSimulation <- function(me)
    {
        ## Return the value of the  variable dt
        return(me$dt)
    }

setDT <- function(me,dt)
    {
        UseMethod("setDT",me)
    }

setDT.default <- function(me,dt)
    {
        ## Not sure what to do so just give up
        return(me)
    }

setDT <- function(me,dt)
    {
        ## set the value of the  variable dt
        me$dt <- dt
        return(me)
    }




# the methods to do the actual simulations.
singleSimulation <- function(simulation,N,T,x0,y0,alpha,beta,gamma,delta,
                             noiseOne,noiseTwo)
  {
    UseMethod("singleSimulation",simulation)
  }

singleSimulation.default <- function(simulation, ...)
    {
      print("singleSimulation.default not defined!")
      return(simulation)
    }

singleSimulation.DiscreteSimulation <-function(
  simulation,N,T,x0,y0,alpha,beta,gamma,delta,noiseOne,noiseTwo)
  {
    ## Make an approximation for one run of the discrete model
    ## with the given parameters. Store the approximation in
    ## the simulation slot when done.

    ## initialize the necessary variables.
    x <- matrix(data=double(N*2),nrow=N,ncol=2)
    x[1,1] <- x0
    x[1,2] <- y0
    lupe <- 2

    ## Go through and make N iterations of the stochastic model.
    while(lupe <= N)
      {
        dW <- rnorm(2,mean=0,sd=1)    # Generate two random numbers
                                      # with a normal dist.
        ## Take one step of the discrete model
        x[lupe,1] <- alpha*x[lupe-1,1] + beta*x[lupe-1,1]*x[lupe-1,2] +
          noiseOne*dW[1]
        x[lupe,2] <- gamma*x[lupe-1,1] + delta*x[lupe-1,2] +
          noiseTwo*dW[2]
        lupe <- lupe + 1
      }

    ## Save the simulation and return the result.
    simulation <- setSimulation(simulation,x)
    return(simulation)
  }

singleSimulation.ContinuousSimulation <- function(
  simulation,N,T,x0,y0,alpha,beta,gamma,delta,noiseOne,noiseTwo)
  {
    ## Make an approximation for one run of the stochastic
    ## differential equation with the given parameters. Store
    ## the approximation in the simulation slot when done.

    ## initialize the necessary variables.
    x <- matrix(data=double(N*2),nrow=N,ncol=2)
    x[1,1] <- x0
    x[1,2] <- y0
    lupe <- 2
    dt <- T/N
    sd <- sqrt(dt)
    simulation <- setDT(simulation,dt)

    ## Go through and make N iterations of the SDE.
    while(lupe <= N)
      {
        dW <- rnorm(2,mean=0,sd=sd)   # Generate two random numbers
                                      # with a normal dist.
        ## Take one step using the Milstein scheme.
        x[lupe,1] <- x[lupe-1,1] +
          (alpha*x[lupe-1,1]*x[lupe-1,2]+beta*x[lupe-1,2])*dt + 
            noiseOne*x[lupe-1,1]*dW[1]  +
              noiseOne*0.5*(dW[1]*dW[1]-dt)
        x[lupe,2] <- x[lupe-1,2] +
          (gamma*x[lupe-1,1]+delta*x[lupe-1,2])*dt +
            noiseTwo*x[lupe-1,2]*dW[2]
        noiseTwo*(dW[2]*dW[2]-dt)
        lupe <- lupe + 1
      }

    ## Save the simulation and return the result.
    simulation <- setSimulation(simulation,x)
    return(simulation)
  }


# the methods to plot the results
plot.DiscreteSimulation <- function(x,y,...)
{
  ## Get the results and plot the x and y values separately.
  results <- getSimulation(x)
  size <- dim(results)
  plot(1:size[1],results[,1],col=2,pch=1,...)
  points(1:size[1],results[,2],col=3,pch=2,...)
}


plot.ContinuousSimulation <- function(x,y,...)
{
  ## Get the results and plot the x and y values separately.
  results <- getSimulation(x)
  size <- dim(results)
  plot(getDT(x)*(0:(size[1]-1)),results[,1],type="l",col=2,pch=1,...)
  points(getDT(x)*(0:(size[1]-1)),results[,2],type="l",col=3,pch=2,...)
}

