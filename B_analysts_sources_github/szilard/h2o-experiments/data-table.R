
library(data.table)

d <- fread("/tmp/d.csv")
## 30 s

colnames(d) <- c("x","y")


system.time(
  d[, mean(y), by=x]
)
## 12 s


system.time(
  d[, .N, by=x]
)
## 6 s


system.time(
  sum(d$y)
)
## 0.2 s
