library(magrittr)
library(data.table)
library(dplyr)
library(igraph)

library(parallel)
cl <- makePSOCKcluster(8)
clusterEvalQ(cl, {
  library(magrittr)
  library(data.table)
  library(dplyr)
  library(igraph)
  invisible(NULL)
})

companies <- readRDS("company-info.Rds") %>%
  data.table
companies %<>% mutate(n = seq_len(nrow(companies)) - 1)
setkey(companies, "id")
stopifnot(is.na(companies$name) %>% sum == 0)
clusterEvalQ(cl, {
  companies <- readRDS("company-info.Rds") %>%
    data.table
  companies %<>% mutate(n = seq_len(nrow(companies)) - 1)
  setkey(companies, "id")
  invisible(NULL)
})

tenders <- readRDS("tenders.Rds")
tenders.id <- 
  tenders %>% # head(1000) %>%
  lapply(`[[`, "tender_company") %>%
  lapply(sapply, `[[`, "id") %>%
  lapply(Filter, f = function(x) grepl("^\\d+$", x)) %>%
  Filter(f = function(x) length(x) > 1)

edges <- parLapply(cl, tenders.id, function(ids) {
  tmp <- companies[ids]
  tmp2 <- tmp$n[!is.na(tmp$n)]
  if (length(tmp2) > 1) combn(tmp2 %>% sort, 2, simplify = FALSE) else NULL
}) %>%
  unlist(recursive = FALSE, use.names = FALSE) %>%
  unique
stopCluster(cl)
edges.node <- unlist(edges) %>% unique
companies <- companies[edges.node+1,]
header.str <- 
'<?xml version="1.0" encoding="UTF-8"?>
<graphml xmlns="http://graphml.graphdrawing.org/xmlns"
         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         xsi:schemaLocation="http://graphml.graphdrawing.org/xmlns
         http://graphml.graphdrawing.org/xmlns/1.0/graphml.xsd">
  <key id="v_name" for="node" attr.name="name" attr.type="string"/>
  <graph id="G" edgedefault="undirected">
'
nodes.str <- sprintf('    <node id="n%d"><data key="v_name">%s</data></node>', 
                    companies$n, companies$id)
edges.str <- sprintf('    <edge source="n%d" target="n%d"></edge>', 
                     sapply(edges, `[`, 1),
                     sapply(edges, `[`, 2))
footer.str <- 
'
  </graph>
</graphml>
'

out.path <- tempfile(fileext = ".graphml")
c(header.str, nodes.str, edges.str, footer.str) %>%
  write(file = out.path)
g <- read_graph(out.path, format = "graphml")
vcount(g)
ecount(g)
transitivity(g, type = "global")
lec <- leading.eigenvector.community(g)
length(lec$membership)
table(lec$membership)

setkey(companies, "id")
companies[V(g)$name[which(lec$membership == 1)]]
# 
# library(intergraph)
# m <- intergraph:::as.matrix.igraph(g, matrix.type = "adjacency", sparse = FALSE)
# 
# library(MCL)
# result <- mcl(m, addLoops = TRUE, ESM = TRUE)
