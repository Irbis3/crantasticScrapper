# add committee co-memberships

comm = data_frame()

# simplify committee names
s$committees = gsub("-", " ", s$committees)
s$committees = gsub("ä", "a", s$committees)
s$committees = gsub("ö|õ", "o", s$committees)
s$committees = gsub("erikomisjon", "komisjon", s$committees)
s$committees = gsub("uurimiskomisjon (.*) sadam", "uurimiskomisjon", s$committees)
s$committees[ s$committees == "" ] = NA
s$committees = tolower(s$committees)

# find unique committees

cat("Parsing committees")
for (i in 11:13) {

  n = s$committees[ s$legislature == i ] %>%
    str_split(";") %>%
    unlist

  comm = rbind(comm, cbind(legislature = i, data.frame(table(n))))

}

# remove a few party abbr. (l. 12)
comm = filter(comm, !n %in% names(colors))
names(comm) = c("legislature", "committee", "members")

cat(":", nrow(comm), "unique categories\n")
write.csv(comm, "data/committees.csv", row.names = FALSE)

for (i in 1:nrow(s)) {

  l = str_split(s$committees[ i ], ";") %>% unlist
  l = comm$legislature == s$legislature[ i ] & comm$committee %in% l
  comm[, paste0(s$legislature[ i ], "_", s$name[ i ]) ] = as.numeric(l)

}

comm$committee[ rowSums(comm[,-1:-3]) != comm$members ]

# assign co-memberships to networks
for (i in unique(comm$legislature)) {

  cat("Legislature", i)

  sp = s$name[ s$legislature == i ]
  names(sp) = paste0(i, "_", s$name[ s$legislature == i ])

  m = comm[ comm$legislature == i, names(comm) %in% names(sp) ]
  cat(":", nrow(m), "committees", ncol(m), "MPs")
  M = m

  m = t(as.matrix(m)) # sponsors in rows, committees in columns
  m = m %*% t(m) # adjacency matrix

  colnames(m) = sp[ colnames(m) ]
  rownames(m) = sp[ rownames(m) ]

  n = get(paste0("net_ee", substr(years[ as.character(i) ], 1, 4)))
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
  assign(paste0("net_ee", substr(years[ as.character(i) ], 1, 4)), n)

  nn %n% "committees" = as.table(rowSums(M))
  assign(paste0("conet_ee", substr(years[ as.character(i) ], 1, 4)), nn)

}
