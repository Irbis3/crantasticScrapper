library(recurrentR)
source(system.file("gen_obj.R", package="recurrentR"))

for(n in c(100,200,300,400)) {
  temp <- gen_obj(n)
  phi_i.y <- recurrentR:::phi_i.y.gen(temp$obj, c(2, -3))
  U.hat <- recurrentR:::U.hat.gen(temp$obj)
  print((U.hat(c(2, -3)) - as.vector(rep(1, n) %*% phi_i.y))  / n)
}

data(obj.list)
phi_i.y <- recurrentR:::phi_i.y.gen(obj.list[[1]], c(2, -3))
U.hat <- recurrentR:::U.hat.gen(obj.list[[1]])
(U.hat(c(2,-3)) - apply(phi_i.y, 2, mean)) / length(obj.list[[1]]@y)
debug(recurrentR:::phi_i.y.gen)
phi_i.y <- recurrentR:::phi_i.y.gen(obj.list[[1]], c(2, -3))
phi_i.y