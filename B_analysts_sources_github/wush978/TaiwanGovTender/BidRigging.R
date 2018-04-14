library(magrittr)
library(data.table)
library(parallel)
cl <- makePSOCKcluster(8)
clusterEvalQ(cl, {
  library(magrittr)
  library(data.table)
})

companies <- readRDS("company-info.Rds") %>%
  data.table
setkey(companies, "id")

clusterEvalQ(cl , {
  companies <- readRDS("company-info.Rds") %>%
    data.table
  setkey(companies, "id")
  invisible(NULL)
})

# BidRigging
tenders <- readRDS("tenders.Rds")
tenders.id <- 
  tenders %>% 
  lapply(`[[`, "tender_company") %>%
  lapply(sapply, `[[`, "id")

tender.magnate <- 
  tenders.id %>% #head(1000) %>%
  parLapply(cl = cl, function(x) {
    id.is_valid <- grepl("^\\d+$", x)
    retval <- character(length(x))
    retval[id.is_valid] <- companies[x[id.is_valid]]$magnate
    retval
  })

## Global Intersection / Union

magnate.score <- 
  tender.magnate %>%
  Filter(f = function(x) length(x) > 1) %>%
  parSapply(cl = cl, function(x) {
    x[is.na(x)] <- ""
    x <- gsub("\\s", "", x)
    x.list <- strsplit(x, ",")
    a <- Reduce(intersect, x.list) %>% length
    b <- Reduce(union, x.list) %>% length
    if (b == 0) 0 else a / b
  })

suspects.name <- which(magnate.score > 1/3 - 1e-5) %>% names
global.result <- data.frame(stringsAsFactors = FALSE,
  name = suspects.name,
  magnate = tender.magnate[suspects.name] %>% sapply(paste, collapse = "----"),
  score = magnate.score[suspects.name]
)

stopCluster(cl)
