# parse sponsor JSON profiles
c = sapply(list.files("raw/sponsors", pattern = "json$", full.names = TRUE),
           fromJSON, flatten = TRUE)

# extract committee membership tables
c = sapply(c, function(x) {
  y = x$id
  x = x$committeeMemberships
  if (class(x) == "data.frame")
    data_frame(chamber = x$committee.council.id, id = y,
               legislature = x$entryDate, code = x$committee.code) %>%
    filter(chamber < 3)
}) %>% bind_rows

# convert chamber ids to letters
c$chamber = c("cn", "cs")[ c$chamber ]

# find legislature id (closest start date)
c$legislature = as.Date(c$legislature)
c$legislature = sapply(c$legislature, function(x) {
  x = x - as.Date(legislatures)
  names(legislatures)[ which.min(x[ x > 0 ]) ]
})

# trim committee codes
c$code = gsub("_$", "", c$code)

# remove baseline legislature
c = filter(c, legislature != "1991-1995")

# export committe membership counts
write.csv(group_by(c, chamber, legislature, code) %>%
            summarise(members = n()), "data/committees.csv", row.names = FALSE)

# unique legislature-committee pairs
c$uid = paste0(c$chamber, c$legislature, c$code)

# master committee membership dataset
comm = data_frame(uid = paste0(c$chamber, c$legislature, c$code)) %>% unique
comm[, as.character(unique(s$id)) ] = 0

for (i in colnames(comm)[ -1 ])
  comm[, i ] = as.integer(comm$uid %in% c$uid[ c$id == i ])

comm$legislature = substr(comm$uid, 1, 6)
for (i in unique(comm$legislature)) {
  
  cat("Legislature", i)
  
  n = get(paste0("net_ch_", i))
  
  sp = network.vertex.names(n)
  names(sp) = gsub("http://www.parlament.ch/f/suche/pages/biografie.aspx\\?biografie_id=", "", n %v% "url")
  
  stopifnot(names(sp) %in% colnames(comm))
  
  m = comm[ grepl(paste0("^", i), comm$legislature), names(comm) %in% names(sp) ]
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
  assign(paste0("net_ch_", i), n)
  
  nn %n% "committees" = as.table(rowSums(M))
  assign(paste0("conet_ch_", i), nn)
  
}
