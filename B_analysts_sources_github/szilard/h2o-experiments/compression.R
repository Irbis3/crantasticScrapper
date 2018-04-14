
library(h2o)

srvx <- h2o.init(startH2O = FALSE) 

generate_frame <- function(type = c("int","double")) {
  type <- match.arg(type)
  int_frac = switch(type, int = 1, double = 0)
  
  h2o.createFrame(srvx, key = "sweep.hex", 
                  rows = 3e8, cols = 1, missing_fraction = 0,
                  categorical_fraction = 0, binary_fraction = 0, integer_fraction = int_frac)
}

dx <- generate_frame("double")
h2o.ls(srvx, "sweep.hex")[1,2]/2^20
## 2300 MB

dx <- generate_frame("int")
h2o.ls(srvx, "sweep.hex")[1,2]/2^20
## 280 MB

