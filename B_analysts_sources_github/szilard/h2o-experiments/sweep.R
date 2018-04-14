
library(h2o)
library(microbenchmark)
 
srvx <- h2o.init(startH2O = FALSE) 

dx <- h2o.createFrame(srvx, key = "sweep.hex", 
        rows = 2e8, cols = 1, missing_fraction = 0,
        categorical_fraction = 0, binary_fraction = 0, integer_fraction = 0)

Sys.sleep(5)

microbenchmark(sum(dx$C1), times = 9)

