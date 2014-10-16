source('relationship.R')

tree <- Relationship()
tree <- readFile(tree,"trees/StreetTrees_KensingtonCedarCottage.csv",skip=2)

tree <- convertDate(tree,"date","%Y%m%d")
tm <- getDataColumn(tree,"date")
timeDefined <- !is.na(tm)




