#

y = runSim( .99, .99, c(0, 0))

#

#
Rprof("sim.prof")
system.time({x = runSim( .99, .9, c(0, 0))})
Rprof(NULL)
head(summaryRprof("sim.prof")$by.self)


# 
g = expand.grid(psi = seq(.8, .99, length = 20),  
                beta1 = seq(-.01, .01, length = 20),  
                beta2 = seq(-.01, .01, length = 20))

