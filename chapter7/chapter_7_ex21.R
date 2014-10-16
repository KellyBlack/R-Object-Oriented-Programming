ls()
one <- 2
ls()
envTwo <- new.env()
assign("two",3,envir=envTwo)
two
attach(envTwo)
ls()
two
detach(envTwo)
two
