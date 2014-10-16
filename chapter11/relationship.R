
source('regression.R')

##################################
## Define the constructor for the
## relationship class
Relationship <- function()
    {
        
        ## Create the list used to represent an
        ## object for this class
        me = list(
            ## reserve the data associated with the class
            data = list(),
            explanatory = character(0),
            response = character(0),
            shortcut = list(
                id="TREE_ID",            # The id of the tree
                street="STD_STREET",     # Name of the street
                side="STREET_SIDE_NAME", # Which side of street
                dia="DIAMETER",          # Diameter of tree
                date="DATE_PLANTED",     # Date tree planted
                barrier="ROOT_BARRIER",  # Y/N ?
                curb="CURB",             # Y/N at curb?
                name="COMMON_NAME"       # Tree type's name
                ),

            theFit = NULL

            
            )

        class(me) <- append(class(me),"Relationship")
        return(me)
    }


getFit.Relationship <- function(theObject)
    {
        ## Return a copy of the data set
        return(getFit(theObject$theFit))
    }


setFit <- function(theObject,newFit)
    {
        ## Set the regression variable
        UseMethod("setFit",theObject)
    }

setFit.default <- function(theObject,newFit)
    {
        ## Set the regression variable
        return(theObject)
    }

setFit.Relationship <- function(theObject,newFit)
    {
        ## Set the regression variable
        theObject$theFit <- newFit
        return(theObject)
    }


getData <- function(theObject)
    {
        ## Return a copy of the data set
        UseMethod("getData",theObject)
    }

getData.default <- function(theObject)
    {
        ## Return a copy of the data set
        return(NA)
    }

getData.Relationship <- function(theObject)
    {
        ## Return a copy of the data set
        return(theObject$data)
    }

setData <- function(theObject,newInformation)
    {
        ## Set the data set to the passed list
        UseMethod("setData",theObject)
    }

setData.default <- function(theObject,newInformation)
    {
        ## Set the data set to the passed list
        return(theObject)
    }

setData.Relationship <- function(theObject,newInformation)
    {
        ## Set the data set to the passed list
        theObject$data <- newInformation
        return(theObject)
    }

getDataColumn <- function(theObject,columnName)
    {
        ## Return a copy of one of the columns from the data set
        UseMethod("getDataColumn",theObject)
    }

getDataColumn.default <- function(theObject,columnName)
    {
        return(NA)
    }

getDataColumn.Relationship <- function(theObject,columnName)
    {
        ## Return a copy of one of the columns from the data set
        ##print(columnName)

        if(columnName %in% names(theObject$shortcut)) {
            ## The name passed is defined in the short cut vector
            ## use value in the short cut to index into the data
            return(theObject$data[[ theObject$shortcut[[columnName]] ]])

        } else if (columnName %in% names(theObject$data)) {
            ## The name itself is a valid name. Use the
            ## column with the same name that is passed.
            return(theObject$data[[ columnName]])
        }

        ## The column name could not be found. Print out an
        ## error message.
        stop(paste("Column",theObject$columnName,"is not defined"))

    }

setDataColumn <- function(theObject,columnName,newColumn)
    {
        ## Set the specified column to the given data set
        UseMethod("setDataColumn",theObject)
    }

setDataColumn.default <- function(theObject,columnName,newColumn)
    {
        ## Set the specified column to the given data set
        return(theObject)
    }

setDataColumn.Relationship <- function(theObject,columnName,newColumn)
    {
        ## Set the specified column to the given data set

        if(columnName %in% names(theObject$shortcut)) {
            ## The name passed is defined in the short cut vector
            ## use value in the short cut to index into the data
            theObject$data[ theObject$shortcut[[columnName]] ] <- newColumn
        } else {
            ## Use the column with the same name that is passed.
            theObject$data[columnName] <- newColumn
        }
        return(theObject)
    }

getShortcut <- function(theObject)
    {
        ## Method to return a copy of the shortcut list
        UseMethod("getShortcut",theObject)
    }

getShortcut.default <- function(theObject)
    {
        ## Method to return a copy of the shortcut list
        return(NA)
    }

getShortcut.Relationship <- function(theObject)
    {
        ## Method to return a copy of the shortcut list
        return(theObject$shortcut)
    }

setShortcut <- function(theObject,newShortcut)
    {
        ## method to set the shortcut.
        UseMethod("setShortcut",theObject)
    }

setShortcut.default <- function(theObject,newShortcut)
    {
        ## method to set the shortcut.
        return(theObject)
    }

setShortcut.Relationship <- function(theObject,newShortcut)
    {
        ## method to set the shortcut.
        theObject$shortcut <- newShortcut
        return(theObject)
    }


addShortcut <- function(theObject,newShortcut,fieldname)
    {
        ## Method to add a new entry to the short cut list
        UseMethod("addShortcut",theObject)
    }

addShortcut.default <- function(theObject,newShortcut,fieldname)
    {
        ## Method to add a new entry to the short cut list
        return(theObject)
    }

addShortcut.Relationship <- function(theObject,newShortcut,fieldname)
    {
        ## Method to add a new entry to the short cut list
        theObject$shortcut[newShortcut] <- fieldname
        return(theObject)
    }

changeShortcut <- function(theObject,newShortcut,fieldname)
    {
        ## This is another name for the addShortcut method.
        return(addShortcut(theObject,newShortcut,fieldname))
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

setExplanatory.Relationship <- function(theObject,newExplanatory)
    {
        ## Method to set the value of the expalantory vector
        theObject$explanatory <- newExplanatory
        return(theObject)
    }

getExplanatory <- function(theObject)
    {
        ## Method to get the value of the expalantory vector
        UseMethod("getExplanatory",theObject)
    }

getExplanatory.default <- function(theObject)
    {
        ## Method to get the value of the expalantory vector
        return(NA)
    }

getExplanatory.Relationship <- function(theObject)
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
        ## Method to set the value of the response 
        return(theObject)
    }

setResponse.Relationship <- function(theObject,newResponse)
    {
        ## Method to set the value of the response 
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

getResponse.Relationship <- function(theObject)
    {
        ## Method to get the value of the response vector
        return(theObject$response)
    }


readFile <- function(theObject,file, ...)
    {
        #print(noquote(paste("read file:",file)))
        UseMethod("readFile",theObject)
    }

readFile.default <- function(theObject,file, ...)
    {
        print("readFile.default not defined!")
        return(theObject)
    }

readFile.Relationship <- function(theObject,file, ...)
    {
        print(noquote(paste("Reading file:",file)))
        arguments <- list(...)
        if(!('skip' %in% names(arguments))) {
          ## there is no skip argument
          ## print out a warning 
          warning("skip is not defined. Defaulting to skip=0")
        }
        theObject <- setData(theObject,read.csv(file,...))
        return(theObject)
    }



convertDate <- function(theObject,columnName,timeFormat)
    {
        #print(noquote(paste("convert time:",timeFormat)))
        UseMethod("convertDate",theObject)
    }

convertDate.default <- function(theObject,columnName,timeFormat)
    {
        print("convertDate.default not defined!")
        return(theObject)
    }

convertDate.Relationship <- function(theObject,columnName,timeFormat)
    {
        timeColumn <- getDataColumn(theObject,columnName)
        theObject  <- setDataColumn(theObject,columnName,
                                    as.Date(as.character(timeColumn),timeFormat))
        return(theObject)
    }



calcRegression <- function(theObject,...)
    {
        UseMethod("calcRegression",theObject)
    }

calcRegression.default <- function(theObject,...)
    {
        print("calcRegression.default not defined!")
        return(theObject)
    }


calcRegression.Relationship <- function(theObject,...)
    {
        ## Method to create a regression object and set it for
        ## this object.
        print("calcRegression.Relationship")
        response    <- getResponse(theObject)
        explanatory <- getExplanatory(theObject)

        if(is.logical(response)) {
            theFit <- LogitRegression(response,explanatory,...)
        } else if (is.integer(response)){
            theFit <- CountRegression(response,explanatory,...)
        } else {
            theFit <- ContinuousRegression(response,explanatory,...)
        }
        theObject <- setFit(theObject,regression(theFit))
        return(theObject)
    }
