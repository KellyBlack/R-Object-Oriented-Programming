source('simulationS3.R')
source('monteCarloS3.R')

monty <- MonteCarlo()
monty <- setParams(monty,
                   100,1,
                   1.0,2.0,
                   1.2,-0.3,0.65,0.2,
                   0.03,0.04)
a <- DiscreteSimulation()
monty <- simulations(monty,500,a)

results <- getValues(monty)
summary(results[,1])
summary(results[,2])
hist(monty,main="Results from a Discrete Simulation")
