source('chapter_8_ex13.R')

plot.Die <- function(theDie,theTitle)
    {
        plot(theDie$getHistory(),
             xlab="Value After A Die Roll",ylab="Frequency",
             main=theTitle)
    }

plot.Coin <- function(theCoin,theTitle)
    {
        plot(theCoin$getHistory(),
             xlab="Value After Coin Flip",ylab="Frequency",
             main=theTitle)
    }

plot(aCoin,"This Here Trial")
plot(aDie,"A More Better Trial")
