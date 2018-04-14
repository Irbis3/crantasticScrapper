
library(h2o)

srvx <- h2o.init(startH2O = FALSE) 

system.time(
  dx <- h2o.importFile(srvx, path = "/tmp/d.csv", key = "d.hex") 
)
## 20 sec

colnames(dx) <- c("x","y")


fun = function(df) { mean(df[,2]) }
h2o.addFunction(srvx, fun)

system.time(
  res <- h2o.ddply(dx, "x", fun)
)
## killed after 5 min running 


system.time(
  h2o.table(dx$x)
)
## 12 sec  


system.time(
  sum(dx$y)
)
## 2 sec

