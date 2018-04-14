
library(h2o)

srvx <- h2o.init(startH2O = FALSE) 

N <- 1e6

dx <- h2o.createFrame(srvx, key = "sweep.hex", seed = 123,
                      rows = N, cols = 2, missing_fraction = 0, integer_range = 1e5/2,
                      categorical_fraction = 0, binary_fraction = 0, integer_fraction = 0.5)
head(dx)


fun <- function(df) { mean(df$C1) }
h2o.addFunction(srvx, fun)

system.time(
  dxaggr <- h2o.ddply(dx, "C2", fun)
)
# user  system elapsed
# 0.030   0.021  88.107


## checks

dck1 <- as.data.frame(dx)
head(as.data.frame(tapply(dck1$C1, dck1$C2, mean)))

dck2 <- as.data.frame(dxaggr)
head(dck2[order(dck2$C2),])

N
nrow(dxaggr)
