library(recurrentR)
data(obj.list)
for(i in seq_along(obj.list)) {
  obj <- obj.list[[i]]
  phi_4i.y <- recurrentR:::phi_4i.y.gen(obj, c(2, -3))
  phi_4i.y[2,3,]
}