
setMethod(f="plot",
          signature="Assignment",
          definition=function(x,maxGrade=NA,div=10,lowerBound=0,...)
          {
            # maxGrade is the assumed maximum grade with respect to calculating the percentage.
            # div is the percentage value to divide the histogram cuts. For example if it
            #     is equal to 10 then the cuts fall on every 10% boundary.
            # lowerBound is the value used to determine which scores to ignore. Anything
            #     strictly less than lowerBound is ignored.

            #print("Plotting an assignment")
            
            values <- GetValue(x)                                   # Get the raw scores
            values <- values[!is.na(values) & values>=lowerBound]   # Remove NA's and anything
                                                                    # below the lowerBound
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
            
            # Make sure that the right endpoint just a little bit
            # bigger to accommodate the largest test scores.
            if(max(values)>=bins[length(bins)]) {
                bins[length(bins)] <- bins[length(bins)]+.1;
            }

            levs <- cut(values,breaks=bins,right=FALSE)  # Convert the data
                                                         # into factors
            gradeFreqs <- table(levs)                    # Determine the
                                                         # frequencies for the
                                                         # different levels.
            top <- max(gradeFreqs)                       # Get the max frequency

            # Plot the histogram.
            histDetails <- hist(values,breaks=bins,
                                freq=TRUE,
                                ylim=c(0,top+2),axes=FALSE,
                                col=grey((seq(length(bins)-1,1,by=-1)/(length(bins)-1))),
                                right=FALSE,...)
            #print(histDetails)

            # Add a box plot across the top
            boxplot(values,horizontal=TRUE,at=top+1.0,add=TRUE,axes=FALSE)

            # Plot the raw data as a strip chart across the bottom
            rug(values)

            # Turn on only the left and lower axes.
            axis(side=1,at=bins)
            axis(side=2,at=seq(0,top+1,by=1))
              
          }
          )


setMethod(f="plot",
          signature="LetterGrade",
          definition=function(x,...)
          {
              # The grades are set of letter grades. Get the letter
              # grades and make a bar plot.
              values <- GetLetterGrade(x)
              values[is.na(values) || (values=="")] <- "NA"  # Get rid of the empty values.
              theNames <- names(GetScale(x))                 # Get the names of the letters.
              frequencies <- table(as.factor(values))        # Get the frequencies of occurances.

              # Create the bar plot.
              barplot(frequencies[theNames],names=theNames,...)
          }
          )


setMethod(f="summary",
          signature="Assignment",
          definition=function(object,...)
          {
              # Get the grade values and return the five point summary.
              values <- GetValue(object)
              return(summary(values))
          }
          )

setMethod(f="summary",
          signature="LetterGrade",
          definition=function(object,...)
          {
              # Get the letter grades as factors and return the frequency table.
              values <- GetLetterGrade(object)
              return(summary(as.factor(values)))
          }
          )


setMethod(f="show",
          signature="Assignment",
          definition=function(object)
          {
              # Print out the grades for an assignment all nice and
              # neat like.
              values <- GetValue(object)
              print(noquote(paste("Assignment:",GetName(object))))
              print(noquote(paste("(",length(values),") Grades:",sep="")))
              print(values)
          }
          )

