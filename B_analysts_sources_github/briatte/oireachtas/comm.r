# add committee co-memberships (finds very, very few)

f = "raw/committees/committees.html"

if (!file.exists(f))
  download.file("http://www.oireachtas.ie/parliament/oireachtasbusiness/committees_list/",
                f, mode = "wb", quiet = TRUE)

h = htmlParse(f) %>%
  xpathSApply("//table//a/@href")

h[ !grepl("^http", h) ] = paste0(root, h[ !grepl("^http", h) ])
h = gsub("ie//parliament", "ie/parliament", h)

raw = data_frame()

for (i in h) {

  f = paste0("raw/committees/", basename(i), ".html")

  if (!file.exists(f))
    download.file(i, f, mode = "wb", quiet = TRUE)

  h = htmlParse(f)
  m = xpathSApply(h, "//table//a[contains(@href, 'MemberID')]/@href")

  if (length(m) > 0)
    raw = rbind(raw, data_frame(
      n = xpathSApply(h, "//title", xmlValue),
      u = i,
      i = m
    ))

}
raw$n = gsub(", Houses of the Oireachtas", "", raw$n)
raw$i = gsub("(.*)MemberID=(\\d+)(.*)", "\\2", raw$i)

write.csv(group_by(raw, n, u) %>% summarise(members = n()),
          "data/committees.csv", row.names = FALSE)

comm = data_frame(u = unique(raw$u))

# add sponsor columns
for (i in list.files("raw/mp-pages"))
  comm[, gsub("mp-|\\.html", "", basename(i)) ] = 0

for (i in colnames(comm)[ -1 ])
  comm[ , i ] = as.numeric(comm$u %in% raw$u[ raw$i == i ])

stopifnot(s$uid %in% names(comm[, -1]))
names(comm)[-1] = paste0("id", names(comm)[-1])

# assign co-memberships to networks
for (i in ls(pattern = "^net_ie")) {

  n = get(i)
  cat(i, ":", network.size(n), "nodes")

  sp = network.vertex.names(n)
  names(sp) = n %v% "url"
  names(sp) = gsub("\\D", "", names(sp))
  names(sp) = paste0("id", names(sp))

  stopifnot(names(sp) %in% colnames(comm))

  m = comm[, names(sp) ]

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
