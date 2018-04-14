# add committee co-memberships

comm = data_frame()

# find unique committees

cat("Parsing committees")
for (i in list.files("raw/mp-pages", full.names = TRUE)) {
  
  h = htmlParse(i)
  l = xpathSApply(h, "//td[@class='biogr_am_ausschuss']//a/@href")
  if (length(l)) {
    
    y = gsub("/PAKT/VHG/(\\w+)/(.*)", "\\1", l)
    n = xpathSApply(h, "//td[@class='biogr_am_ausschuss']//a", xmlValue)
    n = str_clean(n)
    comm = rbind(comm, data_frame(y, n, l))
    
  }
  
}

# drop Bundesrat committees
comm = unique(comm) %>% 
  arrange(y, l, n) %>%
  filter(y %in% names(leg)) %>%
  mutate(y = leg[ y ])

cat(":", nrow(comm), "unique categories\n")

# match to sponsors
# using URLs: same-name committees have different memberships over time

for (i in list.files("raw/mp-pages", full.names = TRUE)) {
  
  h = htmlParse(i)
  l = xpathSApply(h, "//td[@class='biogr_am_ausschuss']//a/@href")
  comm[, paste0("id_", gsub("raw/mp-pages/mp-|\\.html", "", i)) ] = as.numeric(comm$l %in% l)
  
}

stopifnot(names(comm[, -1:-3]) %in% s$id)

# save flat list

names(comm)[1:3] = c("legislature", "committee", "url")
write.csv(cbind(comm[, 1:3 ], members = rowSums(comm[, -1:-3 ])), 
          "data/committees.csv", row.names = FALSE)

# assign co-memberships to networks (no data for l. XIX)
for (i in unique(comm$legislature)) {
  
  cat("Legislature", i)
  
  sp = s$name[ leg[ s$legislature ] == i ]
  names(sp) = s$id[ leg[ s$legislature ] == i ]
  
  m = comm[ comm$legislature == i, names(comm) %in% names(sp) ]
  cat(":", nrow(m), "committees", ncol(m), "MPs")
  M = m
  
  m = t(as.matrix(m)) # sponsors in rows, committees in columns
  m = m %*% t(m) # adjacency matrix
  
  colnames(m) = sp[ colnames(m) ]
  rownames(m) = sp[ rownames(m) ]
  
  n = get(paste0("net_at", years[ as.character(i) ]))
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
  assign(paste0("net_at", years[ as.character(i) ]), n)
  
  nn %n% "committees" = as.table(rowSums(M))
  assign(paste0("conet_at", years[ as.character(i) ]), nn)
  
}
