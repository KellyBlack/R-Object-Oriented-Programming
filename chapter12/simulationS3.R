

######################################################################
## Create the base simulation class
##
## This is used to represent a single simulation
Simulation <- function()
  {

    ## Get the environment for this
    ## instance of the function.
    thisEnv <- environment()

    theSimulation = matrix(0)

    ## Create the list used to represent an
    ## object for this class
    me = list(

      ## Define the environment where this list is defined so
      ## that I can refer to it later.
      thisEnv = thisEnv,

      getSimulation = function()
      {
        ## Return the value of the  variable theSimulation
        return(theSimulation)
      },


      setSimulation = function(theSimulation)
      {
        ## Set the value of the  variable theSimulation
        theSimulation <<- theSimulation
      },

      getFinalValues = function()
      {
        ## Get the value of the data pair at the last time step
        size <- dim(theSimulation)
        return(c(theSimulation[size[1],1],theSimulation[size[1],2]))
      }

      )

    ## Define the value of the list within the current environment.
    assign('this',me,envir=thisEnv)


    ## Set the name for the class
    class(me) <- append(class(me),"Simulation")
    return(me)


  }






######################################################################
## Create a simulation for a discrete simulation.
##
## This is used to represent the results from a discrete simulation.
DiscreteSimulation <- function()
  {
    ## Define the base class and get the environment
    me <- Simulation()
    thisEnv <- me$thisEnv

    ## Save and reserve the number of steps to take.
    assign("N",0,envir=thisEnv)

    ## Set the accessors for the number of steps.
    me$getNumber <- function()
      {
        get("N",envir=thisEnv)
      }

    me$setNumber <- function(number)
      {
        assign("N",number,envir=thisEnv)
      }


    ## Set the name for the class with a numeric grade associated with it.
    class(me) <- append(class(me),"DiscreteSimulation")
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
    thisEnv <- me$thisEnv

    ## assign and reserve the value of the time step.
    assign("dt",0,thisEnv)

    ## Define the accessors for the time step.
    me$getDT <- function()
      {
        ## Return the value of the  variable dt
        return(get("dt",thisEnv))
      }

    me$setDT <- function(dt)
      {
        ## set the value of the  variable dt
        assign("dt",dt,thisEnv)
      }


    ## Set the name for the class with a numeric grade associated with it.
    class(me) <- append(class(me),"ContinuousSimulation")
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
    simulation$setSimulation(x)
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
    simulation$setDT(dt)

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
    simulation$setSimulation(x)
    return(simulation)
  }


# the methods to plot the results
plot.DiscreteSimulation <- function(x,y,...)
{
  ## Get the results and plot the x and y values separately.
  results <- x$getSimulation()
  size <- dim(results)
  plot(1:size[1],results[,1],col=2,pch=1,...)
  points(1:size[1],results[,2],col=3,pch=2,...)
}


plot.ContinuousSimulation <- function(x,y,...)
{
  ## Get the results and plot the x and y values separately.
  results <- x$getSimulation()
  size <- dim(results)
  plot(x$getDT()*(0:(size[1]-1)),results[,1],type="l",col=2,pch=1,...)
  points(x$getDT()*(0:(size[1]-1)),results[,2],type="l",col=3,pch=2,...)
}

