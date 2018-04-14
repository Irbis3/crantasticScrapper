library(recurrentR)
data(obj.list)
Huang2004.list <- list()
for(i in seq_along(obj.list)) {
  obj <- obj.list[[i]]
  Huang2004.list[[i]] <- Huang2004(obj=obj)
}
save(Huang2004.list, file="Huang2004.Rds")