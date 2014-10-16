s <- c("oneSEPtwo","threeSEPfour")
splitted <- strsplit(s,"SEP")
typeof(splitted)
first <- splitted[[1]]
typeof(first)
first
first[2]
