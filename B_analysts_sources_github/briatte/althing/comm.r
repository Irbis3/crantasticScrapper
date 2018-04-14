# add committee co-memberships (unfortunately very approximate)

sponsors = list.files("raw/mp-pages", full.names = TRUE)
raw = data_frame()

for (i in sponsors) {
  
  h = read_html(i, encoding = "UTF-8")
  n = html_nodes(h, xpath = "//p[contains(text(), 'nefnd 19') or contains(text(), 'nefnd 20')]") %>% html_text
  
  if (length(n)) {
    n = unlist(strsplit(n, ","))
    raw = rbind(raw, data_frame(i, n))
  }
  
}

raw$n = str_extract(raw$n, "(\\w+- og )?\\w+nefnd (um (\\w+\\s|\\(seinni\\)\\s)+)?\\d{4}")
raw = subset(raw, !is.na(n))

raw$y = str_extract(raw$n, "\\d{4}")
raw$y[ raw$y %in% 2013:2017 ] = "2013"
raw$y[ raw$y %in% 2009:2012 ] = "2009"
raw$y[ raw$y %in% 2007:2008 ] = "2007"
raw$y[ raw$y %in% 2003:2006 ] = "2003"
raw$y[ raw$y %in% 1999:2002 ] = "1999"
raw$y[ raw$y %in% 1995:1998 ] = "1995"
raw = subset(raw, y > 1994)

raw$i = gsub("\\D", "", raw$i)
raw$n = tolower(gsub("(.*)(\\s\\d{4})+?(.*)", "\\1", raw$n))

# save flat list
write.csv(arrange(raw[, c("y", "n") ], y, n) %>% 
            group_by(y, n) %>% 
            mutate(members = n()) %>% 
            unique, "data/committees.csv", row.names = FALSE)

# unique legislature-committee pairings
raw$u = paste(raw$y, raw$n)

comm = data_frame(u = unique(raw$u))

# add sponsor columns
for (i in sponsors)
  comm[, gsub("mp-|\\.html", "", basename(i)) ] = 0

for (i in colnames(comm)[ -1 ])
  comm[ , i ] = as.numeric(comm$u %in% raw$u[ raw$i == i ])

stopifnot(gsub("\\D", "", s$url) %in% names(comm[, -1]))

# assign co-memberships to networks
for (i in ls(pattern = "^net_is")) {
  
  n = get(i)
  cat(i, ":", network.size(n), "nodes")
  
  sp = network.vertex.names(n)
  names(sp) = n %v% "url"
  names(sp) = gsub("\\D", "", names(sp))
  
  stopifnot(names(sp) %in% colnames(comm))
  
  m = comm[ substr(comm$u, 1, 4) == gsub("\\D", "", i), names(sp) ]
  
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
