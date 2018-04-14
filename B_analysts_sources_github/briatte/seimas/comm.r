# add committee co-memberships

raw = data_frame()
sponsors = list.files("raw/mp-pages", full.names = TRUE)

# find unique committees

cat("Parsing committees")
for (i in sponsors) {

  h = htmlParse(readLines(i, warn = FALSE), asText = TRUE)

  if (grepl("mp-(2012|2008|2004)", i)) {

    n = xpathSApply(h, "//b[text()='Seimo komitetuose']/following-sibling::ul[1]//a", xmlValue)
    n = c(n, xpathSApply(h, "//b[text()='Seimo komitetuose']/following-sibling::ul[2]//a", xmlValue))
    # l = xpathSApply(h, "//b[text()='Seimo komitetuose']/following-sibling::ul[1]//a/@href")
    # l = c(l, xpathSApply(h, "//b[text()='Seimo komitetuose']/following-sibling::ul[2]//a/@href"))

  } else {

    n = xpathSApply(h, "//a[contains(text(), 'komisija') or contains(text(), 'komitetas')]", xmlValue)

  }

  if (length(n))
    raw = rbind(raw, unique(data_frame(i, n)))

}

raw = subset(raw, grepl("komisija|komitetas", n))

cat(":", nrow(unique(raw[, -1 ])), "unique categories\n")

# save flat list
write.csv(raw[, -1 ] %>%
            arrange(n) %>%
            group_by(n) %>%
            mutate(members = n()) %>%
            unique, "data/committees.csv", row.names = FALSE)

# unique committees, using URLs
comm = data_frame(n = unique(raw$n))

# add sponsor columns
raw$i = gsub("raw/mp-pages/mp-|\\.html", "", raw$i)
for (i in sponsors)
  comm[, gsub("raw/mp-pages/mp-|\\.html", "", i) ] = 0

for (i in colnames(comm)[ -1 ])
  comm[, i ] = as.numeric(comm$n %in% raw$n[ raw$i == i ])

stopifnot(paste0(s$legislature, str_replace(s$url, "(.*)p_asm_id=(\\d+)(.*)?", "-\\2")) %in%
            names(comm[, -1]))

# assign co-memberships to networks (missing those for 1992-1996)
for (i in ls(pattern = "^net_lt(1996|20)")) {

  n = get(i)
  cat(i, ":", network.size(n), "nodes")

  sp = network.vertex.names(n)
  names(sp) = n %v% "url"
  names(sp) = paste0(n %n% "years", str_replace(names(sp), "(.*)p_asm_id=(\\d+)(.*)?", "-\\2"))
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
