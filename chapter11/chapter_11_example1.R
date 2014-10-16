source('relationship.R')

tree <- Relationship()
tree <- readFile(tree,"trees/StreetTrees_KensingtonCedarCottage.csv",skip=2)

dia <- getDataColumn(tree,"dia")



