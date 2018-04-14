# add committee co-memberships

sponsors = list.files("raw/mp-pages", pattern = "xml$", full.names = TRUE)
raw = data_frame()

# extract committees from XML files
# full list: http://data.riksdagen.se/sv/koder/?typ=organ&utformat=html

for (i in sponsors) {
  
  h = xmlParse(i)
  n = xpathSApply(h, "//uppgift[contains(text(), 'kottet')]", xmlValue)
  d = xpathSApply(h, "//uppgift[contains(text(), 'kottet')]/../from", xmlValue)
  
  if (length(n))
    raw = rbind(raw, data_frame(i, d, n))
  
}

# impute legislature from general election dates
raw$y = NA
raw$y[ as.Date(raw$d) >= as.Date("1988-09-18") ] = "1988"
raw$y[ as.Date(raw$d) >= as.Date("1991-09-15") ] = "1991"
raw$y[ as.Date(raw$d) >= as.Date("1994-09-18") ] = "1994"
raw$y[ as.Date(raw$d) >= as.Date("1998-09-20") ] = "1998"
raw$y[ as.Date(raw$d) >= as.Date("2002-09-15") ] = "2002"
raw$y[ as.Date(raw$d) >= as.Date("2006-09-17") ] = "2006"
raw$y[ as.Date(raw$d) >= as.Date("2010-09-19") ] = "2010"
raw$y[ as.Date(raw$d) >= as.Date("2014-09-14") ] = "2014"

raw$i = gsub("\\D", "", raw$i)
raw = raw[ !is.na(raw$y), c("i", "y", "n") ]

write.csv(group_by(raw[, -1 ], y, n) %>% 
            arrange(y, n) %>% 
            mutate(members = n()) %>%
            unique, "data/committees.csv", row.names = FALSE)

# unique legislature-committee pairings
raw$u = paste(raw$y, raw$n)

comm = data_frame(u = unique(raw$u))

# add sponsor columns
for (i in sponsors)
  comm[, gsub("raw/mp-pages/mp-|\\.xml", "", i) ] = 0

for (i in colnames(comm)[ -1 ])
  comm[ , i ] = as.numeric(comm$u %in% raw$u[ raw$i == i ])

stopifnot(gsub("\\D", "", s$url) %in% names(comm[, -1]))

# assign co-memberships to networks
for (i in ls(pattern = "^net_se")) {
  
  n = get(i)
  cat(i, ":", network.size(n), "nodes")
  
  sp = network.vertex.names(n)
  names(sp) = gsub("\\D", "", n %v% "url")
  
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
