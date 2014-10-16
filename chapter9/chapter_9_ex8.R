source('antActivity.R')
source('antActivityMethods.R')

ant2 <- SetActivityLevel(ant2,0.1)
ant2@ActivityLevel
ant2 <- SetActivityLevel(ant2,FALSE)
ant2@ActivityLevel
