diameters <- c(28.8, 27.3, 45.8, 34.8, 25.3)
tree <- as.factor(c("pine","pine","oak","pine","oak"))
tapply(diameters,tree,sd)
