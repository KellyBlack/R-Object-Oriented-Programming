source('relationship.R')

# Create a relationship object.
tree <- Relationship()

# Read the file and convert the date column to a date object.
readFile(tree,"trees/StreetTrees_KensingtonCedarCottage.csv",skip=2)
convertDate(tree,"date","%Y%m%d")

# Get a copy of the date column and the side of the street that the
# tree is planted on.
theDate <- tree$getDataColumn("date")
side <- tree$getDataColumn("side")

# Filter out the trees for which the date is not known.
side    <- side[!is.na(theDate)]
theDate <- theDate[!is.na(theDate)]

# Perform the regression.
tree$setExplanatory(theDate)
tree$setResponse(side=="EVEN")
tree <- calcRegression(tree)

# Get the regression and find the details of the results.
fit <- tree$getFit()
fit
summary(fit)
