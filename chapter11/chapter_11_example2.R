source('relationship.R')

tree <- Relationship()
readFile(tree,"trees/StreetTrees_KensingtonCedarCottage.csv",skip=2)

convertDate(tree,"date","%Y%m%d")
tm <- tree$getDataColumn("date")
timeDefined <- !is.na(tm)




