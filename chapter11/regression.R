

##################################
## Define the constructor for the
## regression class
Regression <- function(theResponse,theExplanatory)
    {
        ## Get the environment for this
        ## instance of the function.
        thisEnv <- environment() 

        ## reserve the data associated with the class
        explanatory <- theExplanatory
        response    <- theResponse
        fit <- NULL

        ## Create the list used to represent an
        ## object for this class
        me = list(

            ## Define the environment where this list is defined so
            ## that I can refer to it later.
            thisEnv = thisEnv,

            getFit = function()
            {
                ## Return a copy of the regression object.
                return(fit)
            },

            setFit = function(newFit)
            {
                ## Method to set the value of the expalantory vector
                fit <<- newFit
            },

            setExplanatory = function(newExplanatory)
            {
                ## Method to set the value of the expalantory vector
                explanatory <<- newExplanatory
            },

            getExplanatory = function()
            {
                ## Method to get the value of the expalantory vector
                return(explanatory)
            },

            setResponse = function(newResponse)
            {
                ## Method to set the value of the response vector
                response <<- newResponse
            },

            getResponse = function()
            {
                ## Method to get the value of the response vector
                return(response)
            }

            
            )

        ## Define the value of the list within the current environment.
        assign('this',me,envir=thisEnv)
        
        class(me) <- append(class(me),"Regression")
        return(me)
    }


##################################
## Define the constructor for the
## continuous regression class
ContinuousRegression <- function(theResponse,theExplanatory)
    {

        ## Create the list used to represent an
        ## object for this class
        #print("Making a continuous regression object")
        me = Regression(theResponse,theExplanatory)
        
        class(me) <- append(class(me),"ContinuousRegression")
        return(me)
    }


##################################
## Define the constructor for the
## logit regression class
LogitRegression <- function(theResponse,theExplanatory)
    {

        ## Create the list used to represent an
        ## object for this class
        #print("Making a logit regression object")
        me = Regression(theResponse,theExplanatory)
        
        class(me) <- append(class(me),"LogitRegression")
        return(me)
    }


##################################
## Define the constructor for the
## Poisson regression class
CountRegression <- function(theResponse,theExplanatory)
    {

        ## Create the list used to represent an
        ## object for this class
        #print("Making a Poisson regression object")
        me = Regression(theResponse,theExplanatory)
        
        class(me) <- append(class(me),"CountRegression")
        return(me)
    }


##################################
## Define the functions to perform
## the regression.
##
regression <- function(theObject, ...)
    {
        #print("regression")
        UseMethod("regression",theObject)
    }

regression.default <- function(theObject, ...)
    {
        print("regression.default not defined!")
        return(theObject)
    }


regression.ContinuousRegression <- function(theObject,  ...)
    {
        theObject$setFit(glm(theObject$getResponse() ~ theObject$getExplanatory(),...))
        return(theObject)
    }

regression.LogitRegression <- function(theObject,  ...)
    {
        theObject$setFit(glm(theObject$getResponse() ~ theObject$getExplanatory(),
                             family=binomial(),...))
        return(theObject)
    }

regression.CountRegression <- function(theObject,  ...)
    {
        theObject$setFit(glm(theObject$getResponse() ~ theObject$getExplanatory(),
                             family=poisson(),...))
        return(theObject)
    }

