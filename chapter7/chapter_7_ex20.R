envOne <- new.env()
typeof(envOne)
ls()
ls(envOne)


assign("bubba",12,envir=envOne)
ls()
ls(envOne)
envOne$bubba
get("bubba",envOne)
bubba
