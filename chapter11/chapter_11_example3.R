source('relationship.R')

# Create a relationship object.
tree <- Relationship()

# Read the file and convert the date column to a date object.
tree <- readFile(tree,"trees/StreetTrees_KensingtonCedarCottage.csv",skip=2)
tree <- convertDate(tree,"date","%Y%m%d")

# Get a copy of the date column and the side of the street that the
# tree is planted on.
theDate <- getDataColumn(tree,"date")
side    <- getDataColumn(tree,"side")

# Filter out the trees for which the date is not known.
side    <- side[!is.na(theDate)]
theDate <- theDate[!is.na(theDate)]

# Perform the regression.
tree <- setExplanatory(tree,theDate)
tree <- setResponse(tree,side=="EVEN")
tree <- calcRegression(tree)

# Get the regression and find the details of the results.
fit <- getFit(tree)
fit
summary(fit)
