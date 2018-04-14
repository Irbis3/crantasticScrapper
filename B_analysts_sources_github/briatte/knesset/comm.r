raw = data_frame()

cat("Parsing committee memberships...\n")
for (f in list.files("raw/mps", pattern = "mp-\\d+-en.html", full.names = TRUE)) {

  l = readLines(f, encoding = "ISO-8859-1")
  l = l[ grepl("sBuf\\s\\+=", l) & grepl("table|tr|td", l) ]
  l = gsub("\\ssBuf\\s\\+=\\s", "", l)
  l = gsub("\"", "'", l) %>% paste0(collapse = "")
  l = gsub("';\\t?'", "", l)
  l = gsub("</ta?b?<center>", "</table>", l) # mp-13-en.html
  l = str_extract(l, "<table width=97%(.*?)</table>")
  l = read_html(l, encoding = "ISO-8859-1") %>%
    html_table(header = TRUE, fill = TRUE)
  l = l[[ which(sapply(l, function(x) "Knesset Terms" %in% names(x))) ]][, 1:2 ]
  # legislature LOCF
  for (i in 1:nrow(l)) {
    if (is.na(l[i, 1]) || l[i, 1] == "")
      l[ i, 1] = l[ i - 1, 1]
  }

  # start
  y = which(l[, 1 ] == "Committees") + 1
  if (!length(y))
    y = which(l[, 1 ] == "Knesset Lobbies") + 1

  if (!length(y))
    next

  # end
  x = which(l[, 1 ] == "Other Knesset Roles") - 1
  if (!length(x))
    x = which(l[, 1 ] == "Parliamentary Groups") - 1


  l = l[ seq(y, x), ]
  l = l[ grepl("\\d", l[, 1]), ]

  if (length(l))
    raw = rbind(raw, data_frame(i = f, y = gsub("\\D", "", l[, 1 ]), l = l[, 2 ]))

}

raw = filter(raw, y > 17, nchar(l) > 4, grepl("ommittee|Lobby|Caucus", l))
raw$i = gsub("\\D", "", raw$i)
raw$l = gsub("(.*),\\s(.*?)(*$)?", "\\2", raw$l)
raw$l = gsub("\\*$", "", raw$l)
raw$u = paste(raw$y, raw$l)

# save flat list
write.csv(raw[, c("y", "l") ] %>%
            group_by(y, l) %>%
            mutate(n = n()) %>%
            arrange(y, l) %>%
            unique, "data/committees.csv", row.names = FALSE)

comm = unique(raw[, c("y", "l") ])
comm$u = paste(comm$y, comm$l)

# add sponsor columns
for (i in list.files("raw/mps", pattern = "-en.html", full.names = TRUE))
  comm[, gsub("raw/mps/mp-|-en\\.html", "", i) ] = 0

for (i in 1:nrow(comm)) {

  l = gsub("raw/mps/mp-|\\.html", "", raw$i[ raw$u == comm$u[i] ])
  comm[ i, colnames(comm) %in% l ] = 1

}

comm$y[ comm$y == 18 ] = "2009"
comm$y[ comm$y == 19 ] = "2013"
comm$y[ comm$y == 20 ] = "2015"

for (i in ls(pattern = "^net_il")) {

  cat("Network:", i)

  n = get(i)
  sp = network.vertex.names(n)
  names(sp) = gsub("\\D", "", n %v% "url")

  stopifnot(names(sp) %in% colnames(comm))

  m = filter(comm, y == gsub("\\D", "", i))
  m = m[ , names(m) %in% names(sp) ]
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
  stopifnot(network.size(n) == network.size(nn))

  set.edge.attribute(n, "committee", e$committee)
  assign(i, n)

  nn %n% "committees" = as.table(rowSums(M))
  assign(paste0("co", i), nn)

}
