library(recurrentR)
data(obj.list)
for(i in seq_along(obj.list)) {
  obj <- obj.list[[i]]
  phi_3i.y <- recurrentR:::phi_3i.y.gen(obj, c(2, -3))
}