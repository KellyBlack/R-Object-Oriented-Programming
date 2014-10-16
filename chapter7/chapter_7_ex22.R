one <- 2
changeOne <- function(a)
{
   one <- a
   return(one)
}

changeOne(3)
one

realyChangeOne <- function(a)
{
   one <<- a
   return(one)
}
realyChangeOne(3)
one
