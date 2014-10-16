

######################################################################
# Create the base simulation class
#
# This is used to represent a single simulation
Simulation <- setClass(
    # Set the name for the class
    "Simulation",

    # Define the slots
    slots = c(
        theTime    = "numeric",
        theSimulation = "matrix"
        ),

    # Set the default values for the slots. (optional)
    prototype=list(
        theTime = double(0),
        theSimulation = matrix(0)
        ),

    # Make a function that can test to see if the data is consistent.
    # This is not called if you have an initialize function defined!
    validity=function(object)
    {
        return(TRUE)
    }
    )


# Set the assignment and retrieval methods for the Simulation class.
setGeneric(name="getTime",
           def=function(simulation)
           {
               standardGeneric("getTime")
           }
           )

setMethod(f="getTime",
          signature="Simulation",
          definition=function(simulation)
          {
            ## Return the value of the  variable theTime
              return(simulation@theTime)
          }
          )

setGeneric(name="getSimulation",
           def=function(simulation)
           {
               standardGeneric("getSimulation")
           }
           )

setMethod(f="getSimulation",
          signature="Simulation",
          definition=function(simulation)
          {
            ## Return the value of the  variable theSimulation
              return(simulation@theSimulation)
          }
          )

setGeneric(name="setSimulation",
           def=function(simulation,theSimulation)
           {
               standardGeneric("setSimulation")
           }
           )

setMethod(f="setSimulation",
          signature="Simulation",
          definition=function(simulation,theSimulation)
          {
            ## Set the value of the  variable theTime
              simulation@theSimulation <- theSimulation
              return(simulation)
          }
          )


setGeneric(name="getFinalValues",
           def=function(simulation,theSimulation)
           {
               standardGeneric("getFinalValues")
           }
           )

setMethod(f="getFinalValues",
          signature="Simulation",
          definition=function(simulation)
          {
            ## Get the value of the data pair at the last time step
              theSimulation <- getSimulation(simulation)
              size <- dim(theSimulation)
              return(c(theSimulation[size[1],1],theSimulation[size[1],2]))
          }
          )



######################################################################
# Create a simulation for a discrete simulation.
#
# This is used to represent the results from a discrete simulation.
DiscreteSimulation <- setClass(
    # Set the name for the class with a numeric grade associated with it.
    "DiscreteSimulation",

    # Define the slots
    slots = c(
        N = "numeric"
        ),

    # Set the default values for the slots. (optional)
    prototype=list(N=0),

    # Make a function that can test to see if the data is consistent.
    # (optional)
    # This is not called if you have an initialize function defined!
    validity=function(object)
    {
        if(object@N<0) {
            return("Number of iterations cannot be negative.");
        }
        return(TRUE)
    },

    # This class inherits from the Simulation class
    contains="Simulation"

    )

# Set the assignment and retrieval methods for the DiscreteSimulation class.
setGeneric(name="getNumber",
           def=function(simulation)
           {
               standardGeneric("getNumber")
           }
           )

setMethod(f="getNumber",
          signature="Simulation",
          definition=function(simulation)
          {
            ## Return the value of the  variable N
              return(simulation@N)
          }
          )


######################################################################
# Create a simulation for a continuous simulation.
#
# This is used to represent the results from a continuous simulation.
ContinuousSimulation <- setClass(
    # Set the name for the class with a numeric grade associated with it.
    "ContinuousSimulation",

    # Define the slots
    slots = c(
        dt = "numeric"
        ),

    # Set the default values for the slots. (optional)
    prototype=list(dt=1E-4),

    # Make a function that can test to see if the data is consistent.
    # (optional)
    # This is not called if you have an initialize function defined!
    validity=function(object)
    {
        if(object@dt<0) {
            return("Number of iterations cannot be negative.");
        }
        return(TRUE)
    },

    # This class inherits from the Simulation class
    contains="Simulation"

    )

# Set the assignment and retrieval methods for the ContinuousSimulation class.
setGeneric(name="getDT",
           def=function(simulation)
           {
               standardGeneric("getDT")
           }
           )

setMethod(f="getDT",
          signature="Simulation",
          definition=function(simulation)
          {
            ## Return the value of the  variable dt
              return(simulation@dt)
          }
          )


# the methods to do the actual simulations.
setGeneric(name="singleSimulation",
           def=function(simulation,N,T,x0,y0,alpha,beta,gamma,delta,noiseOne,noiseTwo)
           {
               standardGeneric("singleSimulation")
           }
           )

setMethod(f="singleSimulation",
          signature="DiscreteSimulation",
          definition=function(simulation,N,T,x0,y0,alpha,beta,gamma,delta,noiseOne,noiseTwo)
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
              return(setSimulation(simulation,x))
          }
          )

setMethod(f="singleSimulation",
          signature="ContinuousSimulation",
          definition=function(simulation,N,T,x0,y0,alpha,beta,gamma,delta,noiseOne,noiseTwo)
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

              ## Go through and make N iterations of the SDE.
              while(lupe <= N)
                  {
                      dW <- rnorm(2,mean=0,sd=sd)   # Generate two random numbers
                                                    # with a normal dist.
                      # Take one step using the Milstein scheme.
                      x[lupe,1] <- x[lupe-1,1] +
                        (alpha*x[lupe-1,1]*x[lupe-1,2]+beta*x[lupe-1,2])*dt + 
                        noiseOne*x[lupe-1,1]*dW[1]  +
                          noiseOne*0.5*(dW[1]*dW[1]-dt*dt)
                      x[lupe,2] <- x[lupe-1,2] +
                        (gamma*x[lupe-1,1]+delta*x[lupe-1,2])*dt +
                        noiseTwo*x[lupe-1,2]*dW[2]
                      noiseTwo*(dW[2]*dW[2]-dt*dt)
                      lupe <- lupe + 1
                  }

              ## Save the simulation and return the result.
              return(setSimulation(simulation,x))
          }
          )


# the methods to plot the results
setMethod(f="plot",
          signature="DiscreteSimulation",
          definition=function(x,y,...)
          {
              results <- getSimulation(x)
              size <- dim(results)
              plot(1:size[1],results[,1],col=2,pch=1,...)
              points(1:size[1],results[,2],col=3,pch=2,...)
          }
          )

setMethod(f="plot",
          signature="ContinuousSimulation",
          definition=function(x,y,...)
          {
              results <- getSimulation(x)
              size <- dim(results)
              plot(getDT(x)*(0:(size[1]-1)),results[,1],type="l",col=2,pch=1,...)
              points(getDT(x)*(0:(size[1]-1)),results[,2],type="l",col=3,pch=2,...)
          }
          )
