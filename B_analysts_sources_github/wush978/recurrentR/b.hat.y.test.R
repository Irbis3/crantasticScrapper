library(recurrentR)
data(validate.obj)
obj <- validate.obj
b.hat <- recurrentR:::b.hat.gen(obj)
b.hat.y <- recurrentR:::b.hat.y.gen(obj)
for(i in seq_len(obj@n)) {
  for(j in seq_len(obj@n)) {
    stopifnot(all.equal(b.hat(i)(obj@y[j]), b.hat.y[i,j]))
  }
}
