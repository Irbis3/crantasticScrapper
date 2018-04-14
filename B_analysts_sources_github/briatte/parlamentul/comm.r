# add committee co-memberships

raw = data_frame()
sponsors = list.files("raw/mp-pages", full.names = TRUE)

# find unique committees

cat("Parsing committees")
for (i in sponsors) {
  
  h = htmlParse(i)
  n = xpathSApply(h, "//a[contains(@href, 'idc=')]", xmlValue)
  l = xpathSApply(h, "//a[contains(@href, 'idc=')]/@href")
  if (length(l))
    raw = rbind(raw, unique(data_frame(i, n, l)))
  
}

cat(":", nrow(unique(raw[, -1 ])), "unique categories\n")

# save flat list
write.csv(raw[, -1 ] %>%
            arrange(n, l) %>%
            group_by(n, l) %>%
            mutate(members = n()) %>%
            unique, "data/committees.csv", row.names = FALSE)

# unique committees, using URLs
comm = data_frame(l = unique(raw$l))

# add sponsor columns
for (i in sponsors)
  comm[, gsub("raw/mp-pages/mp-|\\.html", "", i) ] = 0

raw$i = gsub("raw/mp-pages/mp-|\\.html", "", raw$i)

for (i in colnames(comm)[ -1 ])
  comm[ , i ] = as.numeric(comm$l %in% raw$l[ raw$i == i ])

# assign co-memberships to networks
for (i in ls(pattern = "^net_ro")) {
  
  n = get(i)
  cat(i, ":", network.size(n), "nodes")
  
  sp = network.vertex.names(n)
  names(sp) = n %v% "url"
  names(sp) = gsub("(.*)idm=(\\d+)&cam=(\\d)&leg=(\\d+)", "\\3-\\4-\\2", names(sp)) # URL to id
  stopifnot(names(sp) %in% colnames(comm))
  
  m = comm[ , names(sp) ]
  
  cat(" :", nrow(m), "committees", ncol(m), "MPs")
  M = m
  
  m = t(as.matrix(m)) # sponsors in rows, committees in columns
  m = m %*% t(m) # adjacency matrix
  
  stopifnot(ncol(m) == network.size(n))
  colnames(m) = sp[ colnames(m) ]
  rownames(m) = sp[ rownames(m) ]
  
  e = data_frame(i = n %e% "source", j = n %e% "target")
  e$committee = NA
  
  for (j in 1:nrow(e))
    e$committee[ j ] = m[ e$i[ j ], e$j[ j ] ]
  
  cat(" co-memberships:",
      str_pad(paste0(range(e$committee), collapse = "-"), 6, "right"),
      sum(e$committee == 0), "null,",
      sum(e$committee == 1), "single,",
      sum(e$committee > 1), "> 1\n")
  
  nn = network(e[, 1:2], directed = FALSE)
  set.edge.attribute(nn, "committee", e$committee)
  
  print(table(nn %e% "committee", exclude = NULL))
  stopifnot(!is.na(nn %e% "committee"))
  
  set.edge.attribute(n, "committee", e$committee)
  assign(i, n)
  
  nn %n% "committees" = as.table(rowSums(M))
  assign(paste0("co", i), nn)
  
}
