x <- scan("trees.csv",what=list("double","character"),sep=",")
x
x[1]
x[[1]]
typeof(x[1])
typeof(x[[1]])
x[[1]][2]
trees <- as.factor(x[[1]])
trees
