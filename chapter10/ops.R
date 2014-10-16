
setMethod("Ops", signature(e1="NumericGrade", e2="NumericGrade"),
    function(e1, e2) {
        theSum <- callGeneric(GetValue(e1), GetValue(e2))
        return(SetValue(e1,theSum))
    }
)

setMethod("Ops", signature(e1="NumericGrade", e2="numeric"),
    function(e1, e2) {
        theSum <- callGeneric(GetValue(e1), e2)
        return(SetValue(e1,theSum))
    }
)

setMethod("Ops", signature(e1="numeric", e2="NumericGrade"),
    function(e1, e2) {
        theSum <- callGeneric(e1, GetValue(e2))
        return(SetValue(e1,theSum))
    }
)

setMethod("[",
          signature(x="Course",i="ANY"),
          definition=function(x,i=1)
          {
              #print(paste("Get grade item",i))
              return(x@Grades[[i]])
          }
          )

setMethod("[",
          signature(x="Course",i="ANY",j="numeric"),
          definition=function(x,i=1,j=1)
          {
              #print(paste("course value",i,j))
              allGrades <- GetValue(x[i])
              return(allGrades[[j]])
          }
          )

setReplaceMethod("[",
          signature("NumericGrade"),
          definition=function(x,i,value)
          {
              #print(paste("grade value",i,value))
              x@Value[i] = value
              return(x)
          }
          )

setReplaceMethod("[",
                 signature("Course"),
                 definition=function(x,i,j,value)
                 {
                     #print(paste("course grade",i,j,value))
                     grades <- x@Grades[[i]]
                     grades[j] <- value
                     x@Grades[i] <- grades
                     return(x)
                 }
                 )

