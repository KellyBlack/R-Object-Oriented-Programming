a <- "Ladislaus Bortkiewicz"
loc <- gregexpr("Bort",a)
loc
loc[[1]]
loc[[1]][1]
attr(loc[[1]],"match.length")
