# add committee co-memberships

comm = data_frame()

s = read.csv("data/sponsors-se.csv", stringsAsFactors = FALSE)
f = gsub("(.*)dni=(.*)&lng=cz&par_3=(\\d+)", "raw/se/senator-pages/sen-\\3-\\2.html", s$url)

# find unique committees

cat("Parsing committees")
for (i in 1:nrow(s)) {
  
  h = read_html(f[ i ])
  
  l = html_nodes(h, ".membershipModule dd a") %>% html_attr("href")
  n = html_nodes(h, ".membershipModule dd a") %>% html_text
  
  # keep only committees and subcommittees
  k = which(grepl("(V|v)ýbor", n))
  l = l[ k ]
  n = n[ k ]
  
  if (length(l))
    comm = rbind(comm, data_frame(y = s$legislature[ i ], n, l))
  
}

comm = unique(comm) %>% 
  arrange(y, l, n)

cat(":", nrow(comm), "unique categories\n")

for (i in 1:nrow(s)) {
  
  l = read_html(f[ i ]) %>%
    html_nodes(".membershipModule dd a") %>% html_attr("href")
  comm[, paste0(s$legislature[ i ], "_", s$uid[ i ]) ] = as.integer(comm$l %in% l)
  
}

s$uid = paste0(s$legislature, "_", s$uid)
stopifnot(names(comm[, -1:-3]) %in% s$uid)

# save flat list

names(comm)[1:3] = c("legislature", "committee", "url")
write.csv(cbind(comm[, 1:3 ], members = rowSums(comm[, -1:-3])), 
          "data/committees-se.csv", row.names = FALSE)

# assign co-memberships to networks
for (i in ls(pattern = "^net_cz_se\\d{4}")) {
  
  cat("Legislature", i)
  
  n = get(i)
  
  sp = network.vertex.names(n)
  names(sp) = gsub("(.*)par_3=(\\d+)", "\\2", n %v% "url")
  names(sp) = paste0(n %n% "legislature", "_", names(sp))
  
  stopifnot(names(sp) %in% colnames(comm[, -1:-3]))
  
  m = comm[ comm$legislature == n %n% "legislature", names(comm) %in% names(sp) ]
  cat(":", nrow(m), "committees", ncol(m), "MPs")
  M = m
  
  m = t(as.matrix(m)) # sponsors in rows, committees in columns
  m = m %*% t(m) # adjacency matrix
  
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
