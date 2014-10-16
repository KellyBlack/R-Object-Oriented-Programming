source('chapter_8_ex4.R')
source('chapter_8_ex5.R')
source('chapter_8_ex6.R')

oneDie <- list(trials=character(0))
class(oneDie) <- "Die"
simulateGeometric(oneDie,TRUE)

simulateGeometric(oneDie,FALSE)

oneCoin <- list(trials=character(0))
class(oneCoin) <- "Coin"
simulateGeometric(oneCoin,TRUE)
simulateGeometric(oneCoin,FALSE)
