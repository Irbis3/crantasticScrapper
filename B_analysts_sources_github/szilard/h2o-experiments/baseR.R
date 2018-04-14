library(microbenchmark)

x <- runif(2e8)

microbenchmark(sum(x), times = 9)

