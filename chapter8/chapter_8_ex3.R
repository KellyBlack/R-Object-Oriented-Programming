source('chapter_8_ex1.R')
source('chapter_8_ex2.R')
oneDie$trials = c("3","4","1")
oneDie$trials

oneDie <- reset(oneDie)
oneDie

oneCoin$trials = c("H","H","T")
oneCoin <- reset(oneCoin)
oneCoin$trials

# Look at an example that will fail and use the default function.
v <- c(1,2,3)
v <- reset(v)
v
