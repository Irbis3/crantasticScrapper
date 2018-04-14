
library(h2o)

srvx <- h2o.init(startH2O = FALSE) 

dx <- h2o.createFrame(srvx, key = "sweep.hex", 
          rows = 3e8, cols = 1, missing_fraction = 0,
          categorical_fraction = 0, binary_fraction = 0, integer_fraction = 1)

head(dx)
h2o.ls(srvx)
# sweep.hex 300004704

Sys.sleep(3)
for (k in 1:10) print(system.time(sum(dx$C1)))

# user  system elapsed 
# 0.049   0.024   7.802 
# 0.031   0.020   6.405 
# 0.041   0.036   6.255 
# 0.018   0.033   5.930 
# 0.021   0.031   5.273 
# 0.034   0.030   5.594 
# 0.022   0.029   5.092 
# 0.023   0.026   5.208 
# 0.038   0.019   5.728 
# 0.026   0.038   5.271 

Sys.sleep(3)
for (k in 1:10) print(system.time(max(dx$C1)))

# user  system elapsed 
# 0.025   0.025   5.751 
# 0.027   0.029   5.480 
# 0.032   0.015   5.064 
# 0.048   0.008   6.372 
# 0.037   0.010   5.183 
# 0.031   0.015   5.362 
# 0.015   0.040   5.589 
# 0.064   0.043   8.029 
# 0.036   0.020   7.425 
# 0.020   0.034   6.937 
