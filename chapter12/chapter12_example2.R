source('simulationS3.R')
a <- ContinuousSimulation()
a <- singleSimulation(a,100,1,
                      1.2,1.0,
                      0.8,0.4,2.1,-1.5,
                      2.0,1.0)
plot(a,main="One Simulation",xlab="Step",ylab="x,y")

