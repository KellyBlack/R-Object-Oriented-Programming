source('simulationS3.R')
source('monteCarloS3.R')

monty <- MonteCarlo()
monty <- setParams(monty,
                   100,1,
                   1.2,1.0,
                   0.8,0.4,2.1,-1.5,
                   0.1,0.2)

a <- ContinuousSimulation()
monty <- simulations(monty,500,a)
hist(monty,main="Results from a SDE Simulation")
