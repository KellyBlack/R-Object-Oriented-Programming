source('relationship.R')

tree <- Relationship()
readFile(tree,"trees/StreetTrees_KensingtonCedarCottage.csv",skip=2)

dia <- tree$getDataColumn("dia")



