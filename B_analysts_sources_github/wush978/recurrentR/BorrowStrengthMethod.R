library(recurrentR)
library(microbenchmark)

data(obj.list)
obj <- obj.list[[1]]
debug(recurrentR:::BorrowStrengthMethod)
recurrentR:::BorrowStrengthMethod(obj)