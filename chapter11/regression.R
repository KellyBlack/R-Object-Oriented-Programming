

##################################
## Define the constructor for the
## regression class
Regression <- function(theResponse=numeric(0),theExplanatory=numeric(0))
    {

        ## reserve the data associated with the class

        ## Create the list used to represent an
        ## object for this class
        me = list(

            ## Define the environment where this list is defined so
            ## that I can refer to it later.
            explanatory = theExplanatory,
            response    = theResponse,
            fit         = NULL
            )

        class(me) <- append(class(me),"Regression")
        return(me)
    }

getFit <- function(theObject)
    {
        ## Return a copy of the regression object.
        UseMethod("getFit",theObject)
        return(fit)
    }

getFit.default <- function(theObject)
    {
        ## Default function to return the regression object
        return(NA)
    }

getFit.Regression <- function(theObject)
    {
        ## Return a copy of the regression object.
        return(theObject$fit)
    }


setFit <- function(theObject,newFit)
    {
        ## Method to set the value of the expalantory vector
        UseMethod("setFit",theObject)
    }

setFit.default <- function(theObject,newFit)
    {
        ## Method to set the value of the expalantory vector
        return(theObject)
    }

setFit.Regression <- function(theObject,newFit)
    {
        ## Method to set the value of the expalantory vector
        theObject$fit <- newFit
        return(theObject)
    }

setExplanatory <- function(theObject,newExplanatory)
    {
        ## Method to set the value of the expalantory vector
        UseMethod("setExplanatory",theObject)
    }

setExplanatory.default <- function(theObject,newExplanatory)
    {
        ## Method to set the value of the expalantory vector
        return(theObject)
    }

setExplanatory.Regression <- function(theObject,newExplanatory)
    {
        ## Method to set the value of the expalantory vector
        theObject$explanatory <- newExplanatory
        return(theObject)
    }

getExplanatory <- function(theObject)
    {
        ## Method to get the value of the expalantory vector
        UseMethod("getExplanatory",theObject)
        return(explanatory)
    }

getExplanatory.default <- function(theObject)
    {
        ## Method to get the value of the expalantory vector
        return(NA)
    }

getExplanatory.Regression <- function(theObject)
    {
        ## Method to get the value of the expalantory vector
        return(theObject$explanatory)
    }


setResponse <- function(theObject,newResponse)
    {
        ## Method to set the value of the response vector
        UseMethod("setResponse",theObject)
    }

setResponse.default <- function(theObject,newResponse)
    {
        ## Method to set the value of the response vector
        return(theObject)
    }

setResponse.Regression <- function(theObject,newResponse)
    {
        ## Method to set the value of the response vector
        theObject$response <- newResponse
        return(theObject)
    }


getResponse <- function(theObject)
    {
        ## Method to get the value of the response vector
        UseMethod("getResponse",theObject)
    }

getResponse.default <- function(theObject)
    {
        ## Method to get the value of the response vector
        return(NA)
    }

getResponse.Regression <- function(theObject)
    {
        ## Method to get the value of the response vector
        return(theObject$response)
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
        theObject <- setFit(theObject,
                            glm(getResponse(theObject) ~ getExplanatory(theObject),...))
        return(theObject)
    }

regression.LogitRegression <- function(theObject,  ...)
    {
        theObject <- setFit(theObject,
                            glm(getResponse(theObject) ~ getExplanatory(theObject),
                                family=binomial(),...))
        return(theObject)
    }

regression.CountRegression <- function(theObject,  ...)
    {
        theObject <- setFit(theObject,
                            glm(getResponse(theObject) ~ getExplanatory(theObject),
                                family=poisson(),...))
        return(theObject)
    }

