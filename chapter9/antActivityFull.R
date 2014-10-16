
#@article{1996,
#     jstor_articletype = {research-article},
#     title = {Mobile Cellular Automata Models of Ant Behavior: Movement Activity of Leptothorax allardycei},
#     author = {Cole, Blaine J. and Cheshire, David},
#     journal = {The American Naturalist},
#     jstor_issuetitle = {},
#     volume = {148},
#     number = {1},
#     jstor_formatteddate = {Jul., 1996},
#     pages = {pp. 1-15},
#     url = {http://www.jstor.org/stable/2463068},
#     ISSN = {00030147},
#     abstract = {Mobile cellular automata (MCA) models of the activity of ant colonies were used to explore the effects of changing the parameters that govern the types of interactions that can occur between ants. Two parameters have an effect: whether interactions between active ants influence each other's activity and whether interactions between active and inactive ants influence the activity of the inactive ants We then investigated the production of periodic activity in artificial aggregates of workers of Leptothorax allardycei. Using an automated data collection system to analyze the activity patterns of 126 data records of 11.5 h each, we studied the effects of three attributes on the production of periodic activity: the size of the aggregate, the time of day, and the presence of the brood. When the brood was absent, the size of the aggregate had a significant effect on the production of periodic patterns of activity; however, this effect was most pronounced during the day and nearly absent in data records obtained at night. When the brood was present, the time of day had no effect, and the effect of aggregate size was much more pronounced; the extent of periodicity increased linearly with the size of the aggregate All of the experimental results could be reclaimed by altering the parameters of the MCA models. Mobile cellular automata models produce testable predictions that make them especially useful for models of animal behavior.},
#     language = {English},
#     year = {1996},
#     publisher = {The University of Chicago Press for The American Society of Naturalists},
#     copyright = {Copyright © 1996 The University of Chicago},
#    }

rm(list = ls())

# Define the base Ant class.
Ant <- setClass(
    # Set the name of the class
    "Ant",

    # Name the data types (slots) that the class will track
    slots = c(
        Length="numeric",           # the length (size) of this ant.
        
        Position="numeric",         # the position of this ant. (a 3 vector!)
        
        pA="numeric",               # Probability that an ant will transition from 
                                    # active to inactive.

        pI="numeric",               # Probability that an ant will transition from 
                                    # inactive to active.

        ActivityLevel="numeric"     # The ant's current activity level.

        ),

    # Set the default values for the slots. (optional)
    prototype=list(
        Length=4.0,
        Position=c(0.0,0.0,0.0),
        pA=0.05,
        pI=0.1,
        ActivityLevel=0.5
        ),

    # Make a function that can test to see if the data is consistent.
    # (optional)
    # This is not called if you have an initialize function defined!
    validity=function(object)
    {
        # Check to see if the activity level and length is
        # non-negative.
        print("Validity: Ant")
        if(object@ActivityLevel<0.0) {
            return("Error: The activity level is negative")
        } else if (object@Length<0.0) {
            return("Error: The length is negative")
        }
        return(TRUE)
    }
    )


################################
## Create some new ant objects
#ant1 <- new("Ant")
#ant1
#ant2 <- new("Ant",Length=4.5)
#ant2

#ant3 <- Ant(Length=5.0,Position=c(3.0,2.0,1.0))
#ant3

#ant4 <- Ant(Length=-1.0,Position=c(3.0,2.0,1.0))
#ant4


##################################################################
# Now Create the methods for the class. First define the accessors
# for the length.

setGeneric(name="GetLength",
           def=function(antie)
           {
               standardGeneric("GetLength")
           }
           )

setMethod(f="GetLength",
          signature="Ant",
          definition=function(antie)
          {
              return(antie@Length)
          }
          )

setGeneric(name="SetLength",
           def=function(antie,newLength)
           {
               standardGeneric("SetLength")
           }
           )

setMethod(f="SetLength",
          signature="Ant",
          definition=function(antie,newLength)
          {
              if(newLength>0.0) {
                  antie@Length = newLength
              } else {
                  warning("Error - invalid length passed");
              }

              return(antie)
          }
          )

############################################################
# Next define the constructor for the class.
setMethod(f="initialize",
          signature="Ant",
          def=function(.Object,Length=4,Position=c(0.0,0.0,0.0))
          {
              print("Ant initialize")
              .Object = SetLength(.Object,Length)
              .Object = SetPosition(.Object,Position)
              #validObject(.Object)  # you must explicitly call the inspector
              return(.Object)
          }
          )



# Need to define the accessors for the rest of the class!
# Next define the accessors for the position
setGeneric(name="GetPosition",
           def=function(antie)
           {
               standardGeneric("GetPosition")
           }
           )

setMethod(f="GetPosition",
          signature="Ant",
          definition=function(antie)
          {
              return(antie@Position)
          }
          )

setGeneric(name="SetPosition",
           def=function(antie,newPosition)
           {
               standardGeneric("SetPosition")
           }
           )

setMethod(f="SetPosition",
          signature="Ant",
          definition=function(antie,newPosition)
          {
              antie@Position = newPosition
              return(antie)
          }
          )


# Next define the accessors for the probabilities
setGeneric(name="GetProbabilities",
           def=function(antie)
           {
               standardGeneric("GetProbabilities")
           }
           )

setMethod(f="GetProbabilities",
          signature="Ant",
          definition=function(antie)
          {
              return(c(antie@pA,antie@pI))
          }
          )

setGeneric(name="SetProbabilities",
           def=function(antie,newpA,newpI)
           {
               standardGeneric("SetProbabilities")
           }
           )

setMethod(f="SetProbabilities",
          signature="Ant",
          definition=function(antie,newpA,newpI)
          {
              if(newpA>0.0) {
                  antie@pA = newpA
              } else {
                  warning("Error - invalid probability for pA passed");
              }

              if(newpI>0.0) {
                  antie@pI = newpI
              } else {
                  warning("Error - invalid probability for pI passed");
              }

              return(antie)
          }
          )

# Now define the accessors used for the activity level
setGeneric(name="GetActivityLevel",
           def=function(antie)
           {
               standardGeneric("GetActivityLevel")
           }
           )

setMethod(f="GetActivityLevel",
          signature=c("Ant"),
          definition=function(antie)
          {
              return(antie@ActivityLevel)
          }
          )

setGeneric(name="SetActivityLevel",
           def=function(antie,activity)
           {
               standardGeneric("SetActivityLevel")
           }
           )

setMethod(f="SetActivityLevel",
          signature=c("Ant","logical"),
          definition=function(antie,activity)
          {
              if(activity) {
                  antie@ActivityLevel = 0.1
              } else {
                  antie@ActivityLevel = 0.0
              }
              return(antie)
          }
          )

setMethod(f="SetActivityLevel",
          signature=c("Ant","numeric"),
          definition=function(antie,activity)
          {
              if(activity>=0.0) {
                  antie@ActivityLevel = activity
              } else {
                  warning("The activity level cannot be negative")
              }
              return(antie)
          }
          )

#####################################################
# Create the method for calculating the sum of the
# squares of the distances from this ant to the other
# ants passed to it.
setGeneric(name="CalcSumDistances",
           def=function(antie,neighbors)
           {
               standardGeneric("CalcSumDistances")
           }
           )

setMethod(f="CalcSumDistances",
          signature=c("Ant"),
          definition=function(antie,neighbors)
          {
              # Determine the pos. of this ant.
              myPos <- GetPosition(antie)
              meanSquareDistance <- 0.0
              for (ant in neighbors)
                  {
                      # get the pos. of the next ant.
                      # add the square of the distance from this ant.
                      neighborPos <- GetPosition(ant)
                      meanSquareDistance <- meanSquareDistance +
                          sum((myPos-neighborPos)^2)
                  }
              return(meanSquareDistance)
          }
          )


########################################################
# Method set calculate the activity level for an ant.
#
setGeneric(name="DetermineActivityLevel",
           def=function(antie,neighbors)
           {
               standardGeneric("DetermineActivityLevel")
           }
           )

setMethod(f="DetermineActivityLevel",
          signature=c("Ant"),
          definition=function(antie,neighbors)
          {
              return(SetActivityLevel(antie,
                                      CalcActivityLevel(antie,neighbors)))
          }
          )





########################################################
# Now we create the classes that inherit the ant class
#     Ant <- MaleAnt
#       ^--- FemalAnt <- Worker <- Soldier
#

##########################################################
# The first class is the male (drone) class

# Define the male ant class.
MaleAnt <- setClass(
    # Set the name of the class
    "MaleAnt",

    # Name the data types (slots) that the class will track
    slots = c(
        Offspring ="numeric"     # The number of offspring
        ),

    # Set the default values for the slots. (optional)
    prototype=list(
        Offspring=0
        ),

    # Make a function that can test to see if the data is consistent.
    # (optional)
    # This is not called if you have an initialize function defined!
    validity=function(object)
    {
        print("Validity: MaleAnt")
        # Check to see if the number of offspring is non-negative.
        if(object@Offspring<0) {
            return("Error: The number of offspring is negative")
        } 
        return(TRUE)
    },

    # This class inherits from the Ant class
    contains="Ant"
    )


# Create the accessors for this new class
setGeneric(name="SetOffspring",
           def=function(antie,offspring)
           {
               standardGeneric("SetOffspring")
           }
           )

setMethod(f="SetOffspring",
          signature=c("MaleAnt","numeric"),
          definition=function(antie,offspring)
          {
              if(offspring<0) {
                  antie@Offspring = 0
              } else {
                  antie@Offspring = offspring
              }
              return(antie)
          }
          )

setGeneric(name="GetOffspring",
           def=function(antie)
           {
               standardGeneric("GetOffspring")
           }
           )

setMethod(f="GetOffspring",
          signature=c("MaleAnt"),
          definition=function(antie)
          {
              return(antie@Offspring)
          }
          )

# Create the constructor for this class
setMethod(f="initialize",
          signature="MaleAnt",
          def=function(.Object,Length=4,Position=c(0.0,0.0,0.0))
          {
              print("MaleAnt initialize")
              .Object <- callNextMethod(.Object,Length,Position)
              #validObject(.Object)  # you must explicitly call the inspector
              return(.Object)
          }
          )

# Method to calculate the activity level for an ant.
setGeneric(name="CalcActivityLevel",
           def=function(antie,neighbors)
           {
               standardGeneric("CalcActivityLevel")
           }
           )

setMethod(f="CalcActivityLevel",
          signature=c("MaleAnt"),
          definition=function(antie,neighbors)
          {
              sumDistances <- CalcSumDistances(antie,neighbors)
              return(sumDistances/(1.0+sumDistances))
          }
          )




# Create a test drone
male <- MaleAnt(Position=c(-1.0,1.0,1.4))
male <- SetOffspring(male,123)
#print(GetOffspring(male))


##########################################################
# The second class is the female (worker) class

# Define the female ant class.
FemaleAnt <- setClass(
    # Set the name of the class
    "FemaleAnt",

    # Name the data types (slots) that the class will track
    slots = c(
        Food ="numeric"     # The number of food units carried
        ),

    # Set the default values for the slots. (optional)
    prototype=list(
        Food=0
        ),

    # Make a function that can test to see if the data is consistent.
    # (optional)
    # This is not called if you have an initialize function defined!
    validity=function(object)
    {
        print("Validity: FemaleAnt")
        # Check to see if the number of offspring is non-negative.
        if(object@Food<0) {
            return("Error: The number of food units is negative")
        }
        return(TRUE)
    },

    # This class inherits from the Ant class
    contains=c("Ant","VIRTUAL")
    )


# Create the accessors for this new class
setGeneric(name="SetFood",
           def=function(antie,food)
           {
               standardGeneric("SetFood")
           }
           )

setMethod(f="SetFood",
          signature=c("FemaleAnt","numeric"),
          definition=function(antie,food)
          {
              if(food<0) {
                  antie@Food = 0
              } else {
                  antie@Food = food
              }
              return(antie)
          }
          )

setGeneric(name="GetFood",
           def=function(antie)
           {
               standardGeneric("GetFood")
           }
           )

setMethod(f="GetFood",
          signature=c("FemaleAnt"),
          definition=function(antie)
          {
              return(antie@Food)
          }
          )

setMethod(f="initialize",
          signature="FemaleAnt",
          def=function(.Object,Length=4,Position=c(0.0,0.0,0.0))
          {
              print("FemaleAnt initialize ")
              .Object <- callNextMethod(.Object,Length,Position)
              #validObject(.Object)  # you must explicitly call the inspector
              return(.Object)
          }
          )



# Create a test female
female <- FemaleAnt(Position=c(-1.0,1.0,1.4))
female <- SetFood(female,213)
#print(GetFood(female))


##########################################################
# The third class is the worker class

# Define the worker  ant class.
WorkerAnt <- setClass(
    # Set the name of the class
    "WorkerAnt",

    # Name the data types (slots) that the class will track
    slots = c(
        Foraging ="logical",    # Whether or not the ant is actively
                                # looking for food

        Alarm = "logical"       # Whether or not the ant is actively
                                # announcing an alarm.
        
        ),

    # Set the default values for the slots. (optional)
    prototype=list(
        Foraging = FALSE,
        Alarm    = FALSE
        ),

    # Make a function that can test to see if the data is consistent.
    # (optional)
    # This is not called if you have an initialize function defined!
    validity=function(object)
    {
        print("Validity: WorkerAnt")
        return(TRUE)
    },

    # This class inherits from the FemaleAnt class
    contains="FemaleAnt"
    )


# Create the accessors for this new class
setGeneric(name="SetForaging",
           def=function(antie,foraging)
           {
               standardGeneric("SetForaging")
           }
           )

setMethod(f="SetForaging",
          signature=c("WorkerAnt","logical"),
          definition=function(antie,foraging)
          {
              antie@Foraging = foraging
              return(antie)
          }
          )

setGeneric(name="GetForaging",
           def=function(antie)
           {
               standardGeneric("GetForaging")
           }
           )

setMethod(f="GetForaging",
          signature=c("WorkerAnt"),
          definition=function(antie)
          {
              return(antie@Foraging)
          }
          )

setGeneric(name="SetAlarm",
           def=function(antie,alarm)
           {
               standardGeneric("SetAlarm")
           }
           )

setMethod(f="SetAlarm",
          signature=c("WorkerAnt","logical"),
          definition=function(antie,alarm)
          {
              antie@Alarm = alarm
              return(antie)
          }
          )

setGeneric(name="GetAlarm",
           def=function(antie)
           {
               standardGeneric("GetAlarm")
           }
           )

setMethod(f="GetAlarm",
          signature=c("WorkerAnt"),
          definition=function(antie)
          {
              return(antie@Alarm)
          }
          )


setMethod(f="initialize",
          signature="WorkerAnt",
          def=function(.Object,Length=4,Position=c(0.0,0.0,0.0))
          {
              print("WorkerAnt initialize")
              .Object <- callNextMethod(.Object,Length,Position)
              #validObject(.Object)  # you must explicitly call the inspector
              return(.Object)
          }
          )

setMethod(f="CalcActivityLevel",
          signature=c("WorkerAnt"),
          definition=function(antie,neighbors)
          {
              sumDistances <- CalcSumDistances(antie,neighbors)
              return(atan(sumDistances)*2.0/pi)
          }
          )


# Create a test worker
worker <- WorkerAnt(Position=c(-1.0,1.0,1.4))
worker <- SetFood(worker,213)
worker <- SetForaging(worker,TRUE)
#print(GetForaging(worker))


##########################################################
# The fourth class is the soldier class

# Define the female ant class.
SoldierAnt <- setClass(
    # Set the name of the class
    "SoldierAnt",

    # Name the data types (slots) that the class will track
    slots = c(
        Attack ="logical"     # Whether or not the ant is actively
                              # looking to defend its territory

        ),

    # Set the default values for the slots. (optional)
    prototype=list(
        Attack = FALSE
        ),

    # Make a function that can test to see if the data is consistent.
    # (optional)
    # This is not called if you have an initialize function defined!
    validity=function(object)
    {
        print("Validity: SoldierAnt")
        return(TRUE)
    },

    # This class inherits from the FemaleAnt class
    contains="WorkerAnt"
    )


# Create the accessors for this new class
setGeneric(name="SetAttack",
           def=function(antie,attack)
           {
               standardGeneric("SetAttack")
           }
           )

setMethod(f="SetAttack",
          signature=c("SoldierAnt","logical"),
          definition=function(antie,attack)
          {
              antie@Attack = attack
              return(antie)
          }
          )

setGeneric(name="GetAttack",
           def=function(antie)
           {
               standardGeneric("GetAttack")
           }
           )

setMethod(f="GetAttack",
          signature=c("SoldierAnt"),
          definition=function(antie)
          {
              return(antie@Attack)
          }
          )

setMethod(f="initialize",
          signature="SoldierAnt",
          def=function(.Object,Length=4,Position=c(0.0,0.0,0.0))
          {
              print("SoldierAnt initialize")
              .Object <- callNextMethod(.Object,Length,Position)
              #validObject(.Object)  # you must explicitly call the inspector
              return(.Object)
          }
          )

setMethod(f="CalcActivityLevel",
          signature=c("SoldierAnt"),
          definition=function(antie,neighbors)
          {
              sumDistances <- CalcSumDistances(antie,neighbors)
              return(atan(2.0*sumDistances)*2.0/pi)
          }
          )


# Create a test soldier
soldier <- SoldierAnt(Position=c(-1.0,1.0,1.4))
soldier <- SetFood(soldier,213)
soldier <- SetForaging(soldier,TRUE)
soldier <- SetAttack(soldier,TRUE)
#print(GetAttack(soldier))

# Create a vector of ants
workerTest <- WorkerAnt(Position=c(1.0,3.0,5.0))
theAnts <- c(male,worker,soldier)
workerTest <- DetermineActivityLevel(workerTest,theAnts)
