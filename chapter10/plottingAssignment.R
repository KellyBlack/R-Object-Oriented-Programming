source('grades.R')
source('ops.R')

setMethod(f="plot",
          signature="Assignment",
          definition=function(x,maxGrade=NA,div=10,...)
          {

            #print("Plotting an assignment")
            
            values <- GetValue(x) # Get the raw scores
            if(is.na(maxGrade)) {
              # The maxGrade was not set. Assume the max score from
              # the data is the maximum possible.
              maxGrade <- max(values)
              warning(paste("The max gade is not set, and it is assumed to be",
                            maxGrade))
            }

            skip <- maxGrade*div/100; # Set the width of the intervals.
            numLower <- ceiling((maxGrade-min(values))/skip) # Determine the
                                                             # number of intervals.
            # Determine the cut off values between the bins in the histogram.
            bins=c(seq(maxGrade-numLower*skip,max(c(values,maxGrade)),by=skip))
            if(max(bins)<max(values)) {
              # The bins does not include the maximum value. Adjust
              # the bound on the upper most bin.
              bins[length(bins)] <- max(values);
            }
            levs <- cut(values,breaks=bins,right=FALSE)  # Convert the data
                                                         # into factors
            gradeFreqs <- table(levs)                    # Determine the
                                                         # frequencies for the
                                                         # different levels.
            top <- max(gradeFreqs)                       # Get the max frequency

            # Plot the histogram.
            hist(values,breaks=bins,
                 freq=TRUE,
                 ylim=c(0,top+1),axes=FALSE,
                 col=grey((seq(length(bins)-1,1,by=-1)/(length(bins)-1))),
                 right=FALSE,...)

            # Add a box plot across the top
            boxplot(values,horizontal=TRUE,at=top+0.5,add=TRUE,axes=FALSE)

            # Plot the raw data as a strip chart across the bottom
            rug(values)

            # Turn on only the left and lower axes.
            axis(side=1,at=bins)
            axis(side=2,at=seq(0,top+1,by=1))
              
          }
          )


setMethod(f="summary",
          signature="Assignment",
          definition=function(object,...)
          {
              values <- GetValue(object)
              return(summary(values))
          }
          )


setMethod(f="show",
          signature="Assignment",
          definition=function(object)
          {
              values <- GetValue(object)
              print(noquote(paste("Assignment:",GetName(object))))
              print(noquote(paste("(",length(values),") Grades:",sep="")))
              print(values)
          }
          )

