# note: the number of committees was manually set for legislatures 2-6, because
# setting it programmtically would require more doPostBack craziness than I can
# cope with

c = data_frame()

for (i in 133:1) {

  u = paste0("http://www.nrsr.sk/web/Default.aspx?sid=vybory/vybor&ID=", i)
  f = paste0("raw/committee-", i, ".html")

  if (!file.exists(f))
    try(download.file(u, f, mode = "wb", quiet = TRUE), silent = TRUE)

  if (!file.info(f)$size) {

    cat("Failed to download committee", i, "\n")
    file.remove(f)

  }

  h = read_html(f)

  if (html_node(h, "title") %>% html_text == "www.nrsr.sk") {

    cat("Committee", i, "is empty\n")
    next

  }

  u = html_nodes(h, xpath = "//a[contains(@href, 'PoslanecID')]") %>%
    html_attr("href")

  u = u[ !grepl("PoslanecID=0&", u) ]

  if (length(u) < 2) {

    cat("Committee", i, "has no members\n")
    next

  }

  l = gsub("(.*)&CisObdobia=(\\d)", "\\2", u) %>%
          unique

  if (length(l) > 1)
    l = 6

  # make sure all sponsor URLs end by legislature
  u = gsub("&CisObdobia=\\d", "", u)
  u = paste0(u, "&CisObdobia=", l)

  # shortened URL form
  u = gsub("Default.aspx\\?sid=poslanci/poslanec&PoslanecID=", "", u)

  c = rbind(c, data_frame(
    legislature = as.integer(l),
    id = as.integer(i),
    name = html_node(h, "h1") %>% html_text %>% str_trim,
    n_members = length(u),
    members = paste0(u, collapse =  ";")
  ))

}

write.csv(c, "data/committees.csv", row.names = FALSE)

cat("Building co-membership matrix...\n")

comm = data_frame(u = c$id)
sponsors = unlist(strsplit(c$members, ";")) %>% unique

# add sponsor columns
for (i in sponsors)
  comm[, i ] = 0

for (i in colnames(comm)[ -1 ])
  comm[ , i ] = as.numeric(sapply(c$members, strsplit, ";") %>%
                             sapply(function(x) i %in% x))

# sanity check
stopifnot(rowSums(comm[, -1 ]) == c$n_members)

# convert legislature numbers to years
c$legislature = substr(legislatures[ c$legislature ], 1, 4)

# assign co-memberships to networks
for (i in ls(pattern = "^net_sk")) {

  n = get(i)

  sp = network.vertex.names(n)
  names(sp) = gsub("(.*)&PoslanecID=(.*)", "\\2", n %v% "url")

  missing = !names(sp) %in% colnames(comm)
  if (sum(missing) > 0) {

    cat(i, ": adding", sum(missing), "MP(s) with no membership(s)\n")
    for (j in names(sp)[ missing ]) {
      comm[, j ] = 0
    }

  }

  m = comm[ comm$u %in% c$id[ c$legislature == gsub("\\D", "", i) ], names(sp) ]

  cat(i, ":", network.size(n), "nodes", nrow(m), "committees", ncol(m), "MPs")

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
