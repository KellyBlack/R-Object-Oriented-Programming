
source('regression.R')

##################################
## Define the constructor for the
## relationship class
Relationship <- function()
    {
        ## Get the environment for this
        ## instance of the function.
        thisEnv <- environment()
        
        ## reserve the data associated with the class
        data <- list()
        explanatory <- character(0)
        response <- character(0)
        shortcut <- list(
            id="TREE_ID",           # The id of the tree
            street="STD_STREET",     # Name of the street
            side="STREET_SIDE_NAME", # Which side of street
            dia="DIAMETER",          # Diameter of tree
            date="DATE_PLANTED",     # Date tree planted
            barrier="ROOT_BARRIER",  # Y/N ?
            curb="CURB",             # Y/N at curb?
            name="COMMON_NAME"       # Tree type's name
            )

        theFit = NULL

        ## Create the list used to represent an
        ## object for this class
        me = list(

            ## Define the environment where this list is defined so
            ## that I can refer to it later.
            thisEnv = thisEnv,

            getFit = function()
            {
                ## Return a copy of the data set
                return(theFit$getFit())
            },

            setFit = function(newFit)
            {
                ## Set the regression variable
                theFit <<- newFit
            },

            getData = function()
            {
                ## Return a copy of the data set
                return(data)
            },

            setData = function(newInformation)
            {
                ## Set the data set to the passed list
                data <<- newInformation
            },

            getDataColumn = function(columnName)
            {
                ## Return a copy of one of the columns from the data set
                #print(columnName)

                if(columnName %in% names(shortcut)) {
                    ## The name passed is defined in the short cut vector
                    ## use value in the short cut to index into the data
                    return(data[[ shortcut[[columnName]] ]])

                } else if (columnName %in% names(data)) {
                    ## The name itself is a valid name. Use the
                    ## column with the same name that is passed.
                    return(data[[ columnName]])
               }

                ## The column name could not be found. Print out an
                ## error message.
                stop(paste("Column",columnName,"is not defined"))

            },

            setDataColumn = function(columnName,newColumn)
            {
                ## Set the specified column to the given data set

                if(columnName %in% names(shortcut)) {
                    ## The name passed is defined in the short cut vector
                    ## use value in the short cut to index into the data
                    data[ shortcut[[columnName]] ] <<- newColumn
                } else {
                    ## Use the column with the same name that is passed.
                    data[columnName] <<- newColumn
               }
            },

            getShortcut = function()
            {
                ## Method to return a copy of the shortcut list
                return(shortcut)
            },
            
            setShortcut = function(newShortcut)
            {
                ## method to set the shortcut.
                shortcut <<- newShortcut
            },

            addShortcut = function(newShortcut,fieldname)
            {
                ## Method to add a new entry to the short cut list
                shortcut[newShortcut] <<- fieldname
            },

            changeShortcut = function(newShortcut,fieldname)
            {
                ## This is another name for the addShortcut method.
                this <- get('this',thisEnv)
                this$addShortcut(newShortcut,fieldname)
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
        
        class(me) <- append(class(me),"Relationship")
        return(me)
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
        theObject$setData(read.csv(file,...))
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
        timeColumn <- theObject$getDataColumn(columnName)
        theObject$setDataColumn(columnName,
                                as.Date(as.character(timeColumn),timeFormat))
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
        response    <- theObject$getResponse()
        explanatory <- theObject$getExplanatory()

        if(is.logical(response)) {
            theFit <- LogitRegression(response,explanatory,...)
        } else if (is.integer(response)){
            theFit <- CountRegression(response,explanatory,...)
        } else {
            theFit <- ContinuousRegression(response,explanatory,...)
        }
        theObject$setFit(regression(theFit))
        return(theObject)
    }
