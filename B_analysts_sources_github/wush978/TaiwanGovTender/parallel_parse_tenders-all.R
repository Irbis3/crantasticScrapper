library(logging)
basicConfig()
retval <- list()
for(.rank in 0:59) {
  retval.path <- sprintf("parallel_parse_tenders-%03d.Rds", .rank)
  loginfo(sprintf("Reading archive %s ...", retval.path))
  retval[[.rank + 1]] <- readRDS(retval.path)
}
retval.all <- unlist(retval, recursive = FALSE)
saveRDS(retval.all, "tenders.Rds")

