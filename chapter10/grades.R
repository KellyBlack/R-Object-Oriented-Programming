######################################################################
# Create the base assignment class
#
# This is used to represent the grades for one assignment.
Assignment <- setClass(
    # Set the name for the class
    "Assignment",

    # Define the slots
    slots = c(
        Name = "character",
        Number = "numeric"
        ),

    # Set the default values for the slots. (optional)
    prototype=list(
        Name = "Test",
        Number = as.integer(1)
        ),

    # Make a function that can test to see if the data is consistent.
    # This is not called if you have an initialize function defined!
    validity=function(object)
    {
        if(object@Number < 0) {
            object@Number <- 0
            warning("A negative number for the assignment number is passed. It is set to zero.")
        }
        return(TRUE)
    }
    )


# Set the assignment and retrieval methods for the Assignment class.
setGeneric(name="GetName",
           def=function(assignment)
           {
               standardGeneric("GetName")
           }
           )

setMethod(f="GetName",
          signature="Assignment",
          definition=function(assignment)
          {
              return(assignment@Name)
          }
          )

setGeneric(name="SetName",
           def=function(assignment,name)
           {
               standardGeneric("SetName")
           }
           )

setMethod(f="SetName",
          signature="Assignment",
          definition=function(assignment,name)
          {
              assignment@Name <- name
              return(assignment)
          }
          )


setGeneric(name="GetNumber",
           def=function(assignment)
           {
               standardGeneric("GetNumber")
           }
           )

setMethod(f="GetNumber",
          signature="Assignment",
          definition=function(assignment)
          {
              return(assignment@Number)
          }
          )

setGeneric(name="SetNumber",
           def=function(assignment,number)
           {
               standardGeneric("SetNumber")
           }
           )

setMethod(f="SetNumber",
          signature="Assignment",
          definition=function(assignment,number)
          {
              assignment@Number <- as.integer(number)
              return(assignment)
          }
          )



######################################################################
# Create the class to keep track of the grades that are numeric in
# nature.
#
# This is used to represent the numeric grades for one assignment.
NumericGrade <- setClass(
    # Set the name for the class with a numeric grade associated with it.
    "NumericGrade",

    # Define the slots
    slots = c(
        Value = "numeric"
        ),

    # Set the default values for the slots. (optional)
    prototype=list(
        Value = 0.0
        ),

    # Make a function that can test to see if the data is consistent.
    # (optional)
    # This is not called if you have an initialize function defined!
    validity=function(object)
    {
        if(object@Value < 0) {
            object@Value <- 0
            warning("A negative number for the assignment value is passed. It is set to zero.")
        }
        return(TRUE)
    },

    # This class inherits from the Assignment class
    contains="Assignment"

    )

# Create the methods to retrieve and set the values of the NumericGrade class.
setGeneric(name="GetValue",
           def=function(assignment)
           {
               standardGeneric("GetValue")
           }
           )

setMethod(f="GetValue",
          signature="NumericGrade",
          definition=function(assignment)
          {
              return(assignment@Value)
          }
          )

setGeneric(name="SetValue",
           def=function(assignment,value)
           {
               standardGeneric("SetValue")
           }
           )

setMethod(f="SetValue",
          signature="NumericGrade",
          definition=function(assignment,value)
          {
              assignment@Value <- value
              return(assignment)
          }
          )


setGeneric(name="GradeReport",
           def=function(assignment,maxGrade=NA,div=10)
           {
               standardGeneric("GradeReport")
           }
           )

setMethod(f="GradeReport",
          signature="Assignment",
          definition=function(assignment,maxGrade=NA,div=10)
          {
              print(noquote(''))
              print(noquote(paste("Grade report for ",GetName(assignment))))
              print(noquote(''))
              print(summary(assignment))         # Print out a five point summary for the data
              values <- GetValue(assignment)     # Get the raw scores.

              if(is.na(maxGrade)) {
                  # The maxGrade was not set. Assume the max score from
                  # the data is the maximum possible.
                  maxGrade <- max(values)
                  warning(paste("The max gade is not set, and it is assumed to be",
                                maxGrade))
              }
              
              skip <- maxGrade*div/100;                        # Set the width of the intervals.

              # Move the max grade up to make sure that the left sided
              # cut will have an interval to contain the top scores.
              while(maxGrade<=max(values))
                  {
                      maxGrade = maxGrade + skip
                  }
              
              numLower <- ceiling((maxGrade-min(values))/skip) # Determine the
                                                               # number of intervals.

              # Determine all of the cutoff points
              bins=c(seq(maxGrade-numLower*skip,max(c(values,maxGrade)),by=skip))
              levs <- cut(values,breaks=bins,right=FALSE)  # Convert the data
                                                           # into factors
              gradeFreqs <- table(levs)                    # Determine the
                                                           # frequencies for the
                                                           # different levels.

              print(noquote(''))
              print(noquote("Stem Leaf plot of grades:"))
              print(stem(values))
              print(noquote(''))
              
              print(noquote("Grade Frequencies:"))
              print(gradeFreqs)
              print(noquote(''))

              print(noquote("Sorted Grades:"))
              print(sort(values))
          }
          )



######################################################################
# Create the class to keep track of the grades that are letters in
# nature.
#
# This is used to represent the letter grades for one assignment.
LetterGrade <- setClass(
    # Set the name for the class with a numeric grade associated with it.
    "LetterGrade",

    # Define the slots
    slots = c(
        Value = "character",
        Scale = "list"
        ),

    # Set the default values for the slots. (optional)
    prototype=list(
        Value = "F",
        Scale = list(
            'A+'=98,'A'=95,'A-'=92,
            'B+'=88,'B'=85,'B-'=83,
            'C+'=78,'C'=75,'C-'=73,
            'D+'=68,'D'=65,'D-'=63,
            'F+'=58,'F'=55,'F-'=53,
            "NA"=0)
        ),

    # Make a function that can test to see if the data is consistent.
    # (optional)
    # This is not called if you have an initialize function defined!
    validity=function(object)
    {
        pos <- grep(paste(object@Value,"$",sep=""),names(object@Scale))
        if(length(pos) != 1) {
            object@Value <- 'F-'
            warning("The grade is not recognized.")
        }
        return(TRUE)
    },

    # This class inherits from the Assignment class
    contains="Assignment"

    )

# Define the methods to retrieve or set the values for a LetterGrade object.
setGeneric(name="GetScale",
           def=function(assignment,scale)
           {
               standardGeneric("GetScale")
           }
           )

setMethod(f="GetScale",
          signature="LetterGrade",
          definition=function(assignment)
          {
              return(assignment@Scale)
          }
          )

setGeneric(name="SetScale",
           def=function(assignment,scale)
           {
               standardGeneric("SetScale")
           }
           )

setMethod(f="SetScale",
          signature="LetterGrade",
          definition=function(assignment,scale)
          {
              assignment@Scale <- scale
              return(assignment)
          }
          )

setMethod(f="GetValue",
          signature="LetterGrade",
          definition=function(assignment)
          {
              return(unlist(assignment@Scale[assignment@Value]))
          }
          )


setGeneric(name="GetLetterGrade",
           def=function(assignment,scale)
           {
               standardGeneric("GetLetterGrade")
           }
           )

setMethod(f="GetLetterGrade",
          signature="LetterGrade",
          definition=function(assignment)
          {
              return(unlist(assignment@Value))
          }
          )


setMethod(f="SetValue",
          signature="LetterGrade",
          definition=function(assignment,value)
          {
              # Loop through each item in the vector of values. Also,
              # convert the value to a character vector. We need the
              # scale list inside the loop so grab a copy now for
              # later use.
              lupe <- 1
              value <- as.character(value)
              theScale <- GetScale(assignment)
              theNames <- names(theScale)
              while(lupe <= length(value))
                  {
                      # Determine if this item can be found in the
                      # list of scale items. Express it as a regular
                      # expression and make sure it is an exact match
                      # by using first and last place markers.
                      thePattern <- paste("^",sub("\\+","\\\\+",value[lupe]),"$",sep="")
                      pos <- grep(thePattern,theNames)
                      if(value[lupe]=="") {
                          #An empty string was passed.
                          value[lupe] <- "NA"
                      } else if(length(pos) != 1) {
                          # This item was not found. Print a warning and make it an NA
                          warning(paste("The grade \"",value[lupe],"\" is not recognized. It is set to NA.",sep=""))
                          value[lupe] <- "NA"
                      } 
                      lupe <- lupe + 1
                  }
              assignment@Value <- value
              return(assignment)
          }
          )



setMethod(f="GradeReport",
          signature="LetterGrade",
          definition=function(assignment,maxGrade=NA,div=10)
          {
              print(noquote(''))
              print(noquote(paste("Grade report for ",GetName(assignment))))
              print(noquote(''))
              print(summary(assignment))         # Print out a five point summary for the data
          }
          )





######################################################################
# Create the Course class to keep track of all grades
Course <- setClass(
    # Set the name for the class
    "Course",

    # Define the slots
    slots = c(
        GradesFile = "character",
        GradeTypes = "character",
        Grades     = "list"
        ),

    # Set the default values for the slots. (optional)
    prototype=list(
        GradesFile = "",
        GradeTypes = c("test","hw","quiz","project"),
        Grades     = list()
        ),

    # Make a function that can test to see if the data is consistent.
    # This is not called if you have an initialize function defined!
    validity=function(object)
    {
        return(TRUE)
    }
    )


# Define the methods used to retrieve or set the values within a Course object.
setGeneric(name="GetFileName",
           def=function(course)
           {
               standardGeneric("GetFileName")
           }
           )

setMethod(f="GetFileName",
          signature="Course",
          definition=function(course)
          {
              return(course@GradesFile)
          }
          )

setGeneric(name="SetFileName",
           def=function(course,fileName)
           {
               standardGeneric("SetFileName")
           }
           )

setMethod(f="SetFileName",
          signature="Course",
          definition=function(course,fileName)
          {
              course@GradesFile = fileName
              return(course)
          }
          )

setGeneric(name="GetGrades",
           def=function(course)
           {
               standardGeneric("GetGrades")
           }
           )

setMethod(f="GetGrades",
          signature="Course",
          definition=function(course)
          {
              return(course@Grades)
          }
          )

setGeneric(name="SetGrades",
           def=function(course,grades)
           {
               standardGeneric("SetGrades")
           }
           )

setMethod(f="SetGrades",
          signature="Course",
          definition=function(course,grades)
          {
              course@Grades = grades
              return(course)
          }
          )


setGeneric(name="GetGradeTypes",
           def=function(course)
           {
               standardGeneric("GetGradeTypes")
           }
           )

setMethod(f="GetGradeTypes",
          signature="Course",
          definition=function(course)
          {
              return(course@GradeTypes)
          }
          )

setGeneric(name="SetGradeTypes",
           def=function(course,gradeTypes)
           {
               standardGeneric("SetGradeTypes")
           }
           )

setMethod(f="SetGradeTypes",
          signature="Course",
          definition=function(course,gradeTypes)
          {
              course@GradeTypes = gradeTypes
              return(course)
          }
          )



setGeneric(name="ReadGrades",
           def=function(course)
           {
               standardGeneric("ReadGrades")
           }
           )

setMethod(f="ReadGrades",
          signature="Course",
          definition=function(course)
          {
              grades <- read.csv(GetFileName(course))
              convertedGrades <- list()
              courseTypes <- GetGradeTypes(course)
              for (gradeItem in names(grades))
              {
                  # Go through each column from the file.
                  for (type in courseTypes)
                  {
                      # go through each course type and determine if
                      # this column is a quiz/test/hw/?
                      if(length(grep(type,gradeItem))>0)
                          {
                              # The prefix for the name matches one of the predefined types.
                              if((class(grades[[gradeItem]])=="numeric") ||
                                 (class(grades[[gradeItem]])=="integer")) {
                                  thisItem <- NumericGrade()
                                  #print(paste("This item,",gradeItem,",is a numeric grade item.",class(thisItem)))
                              } else {
                                  thisItem <- LetterGrade()
                                  #print(paste("This item,",gradeItem,", is a letter grade.",class(thisItem)))
                              }
                              # Convert the values into their respective grades.
                              thisItem <- SetValue(thisItem,grades[[gradeItem]])
                              #print(paste("class: ",class(thisItem)))
                              convertedGrades[gradeItem] <- thisItem
                          }
                  }
              }
              return(SetGrades(course,convertedGrades))
          }
          )

