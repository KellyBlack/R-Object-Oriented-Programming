
source('simulationS3.R')
a <- DiscreteSimulation()
a <- singleSimulation(a,100,100,
                      1.0,2.0,
                      1.2,-0.3,0.65,0.2,
                      0.03,0.04)
plot(a,main="One Simulation",xlab="Step",ylab="x,y")
