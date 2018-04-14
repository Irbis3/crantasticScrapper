ll = c("1992-1996", "1996-2000", "2000-2004",
       "2004-2008", "2008-2012", "2012-2016")

# au = unlist(strsplit(b$authors, ",\\s"))
# table(toupper(au) %in% s$id)
# table(au[ !toupper(au) %in% s$id ])
#
# co = unlist(strsplit(b$cosponsors, ", ")) %>% na.omit
# table(toupper(co) %in% toupper(s$name))
# table(co[ !toupper(co) %in% toupper(s$name) ])

for (ii in unique(b$legislature) %>% sort %>% rev) {

  cat(ii)

  data = subset(b, legislature == ii)
  sp = subset(s, legislature == ii)

  m = unlist(strsplit(data$authors, ",\\s?"))
  m = m[ !toupper(m) %in% s$id ]
  if (length(m))
    cat(" [missing", n_distinct(m), "authors]")

  # convert all authors to full names
  rownames(sp) = sp$id
  data$authors = sapply(toupper(data$authors), function(x) {
    x = strsplit(x, ",\\s?") %>% unlist %>% str_trim
    paste0(na.omit(sp[ x, "name" ]), collapse = ";")
  })

  m = unlist(strsplit(b$cosponsors, ", ")) %>% na.omit
  m = m[ !toupper(m) %in% toupper(s$name) ]
  if (length(m))
    cat(" [missing", n_distinct(m), "cosponsors]")

  # convert all cosponsors to full names
  rownames(sp) = toupper(sp$name)
  data$cosponsors = sapply(toupper(data$cosponsors), function(x) {
    x = strsplit(x, ",\\s?") %>% unlist %>% str_trim
    paste0(na.omit(sp[ x, "name" ]), collapse = ";")
  })

  # drop bills with no sponsor information at all
  data = filter(data, nchar(authors) > 1 | nchar(cosponsors) > 1)

  # assign all cosponsors as first authors when the latter are missing
  m = (data$authors == "")
  data$authors[ m ] = data$cosponsors[ m ]

  # check whether cosponsors contain all first authors in the same order
  m = (data$cosponsors == "")
  m[ !m ] = str_detect(data$cosponsors[ !m ], data$authors[ !m ])

  # the two sponsor columns are compatible with each other, except for a
  # few rows in the current legislature; generally speaking, because the
  # 'cosponsors' column corresponds to the 'more authors' page, it should
  # be more accurate than the 'authors' column, which misses a few names
  stopifnot(all(m) | ii == "2012-2016")

  # use 'authors' (bills with no "more authors..." page) or 'cosponsors'
  data$sponsors = ifelse(data$cosponsors == "", data$authors, data$cosponsors)

  # create separate dataset for cosponsored bills
  data$n_au = 1 + str_count(data$sponsors, ";")
  bills = filter(data, n_au > 1)

  cat(":", nrow(bills), "cosponsored documents, ")

  # ============================================================================
  # DIRECTED EDGE LIST
  # ============================================================================

  edges = lapply(bills$sponsors, function(d) {

    w = unlist(strsplit(d, ";"))
    d = expand.grid(i = w, j = w[1], stringsAsFactors = FALSE)

    return(data.frame(d, w = length(w) - 1)) # number of cosponsors

  }) %>% bind_rows

  # ============================================================================
  # EDGE WEIGHTS
  # ============================================================================

  # first author self-loops, with counts of cosponsors
  self = subset(edges, i == j)

  # count number of bills per first author
  n_au = table(self$j)

  # remove self-loops from directed edge list
  edges = subset(edges, i != j)

  # count number of bills cosponsored per sponsor
  n_co = table(edges$i)

  # identify directed ties
  edges$ij = apply(edges[, 1:2 ], 1, paste0, collapse = "///")

  # raw edge counts
  raw = table(edges$ij)

  # Newman-Fowler weights (weighted quantity of bills cosponsored)
  edges = aggregate(w ~ ij, function(x) sum(1 / x), data = edges)

  # expand to edge list
  edges = data_frame(i = gsub("(.*)///(.*)", "\\1", edges$ij),
                     j = gsub("(.*)///(.*)", "\\2", edges$ij),
                     raw = as.vector(raw[ edges$ij ]), # raw edge counts
                     nfw = edges$w)

  # Gross-Shalizi weights (weighted propensity to cosponsor)
  edges = merge(edges, aggregate(w ~ j, function(x) sum(1 / x), data = self))
  edges$gsw = edges$nfw / edges$w

  # sanity check
  stopifnot(edges$gsw <= 1)

  # final edge set: cosponsor, first author, weights
  edges = select(edges, i, j, raw, nfw, gsw)

  cat(nrow(edges), "edges, ")

  # ============================================================================
  # DIRECTED NETWORK
  # ============================================================================

  n = network(edges[, 1:2 ], directed = TRUE)

  n %n% "country" = meta[ "cty" ] %>% as.character
  n %n% "lang" = meta[ "lang" ] %>% as.character
  n %n% "years" = ii %>% as.character
  n %n% "legislature" = as.character(which(ll == ii) + 5)
  n %n% "chamber" = meta[ "ch" ] %>% as.character
  n %n% "type" = meta[ "type" ] %>% as.character
  n %n% "ipu" = meta[ "ipu" ] %>% as.integer
  n %n% "seats" = meta[ "seats" ] %>% as.integer

  n %n% "n_cosponsored" = nrow(bills)
  n %n% "n_sponsors" = table(data$n_au) # already subset to legislature

  # ============================================================================
  # VERTEX-LEVEL ATTRIBUTES
  # ============================================================================

  n_au = as.vector(n_au[ network.vertex.names(n) ])

  n %v% "n_au" = ifelse(is.na(n_au), 0, n_au)

  n_co = as.vector(n_co[ network.vertex.names(n) ])
  n %v% "n_co" = ifelse(is.na(n_co), 0, n_co)

  n %v% "n_bills" = n %v% "n_au" + n %v% "n_co"

  cat(network.size(n), "nodes\n")

  rownames(sp) = sp$name

  n %v% "url" = sp[ network.vertex.names(n), "url" ]
  n %v% "sex" = sp[ network.vertex.names(n), "sex" ]
  n %v% "born" = sp[ network.vertex.names(n), "born" ]
  n %v% "party" = sp[ network.vertex.names(n), "party" ]
  n %v% "partyname" = groups[ n %v% "party" ] %>% as.character
  n %v% "constituency" = sp[ network.vertex.names(n), "constituency" ]
  n %v% "lr" = scores[ n %v% "party" ] %>% as.numeric
  n %v% "photo" = sp[ network.vertex.names(n), "photo" ]
  # mandate years done up to start year of legislature, imputed for all networks
  # except the last one, using past sponsor profiles
  sp$nyears = sapply(sp$mandates, function(x) {
    sum(unlist(strsplit(x, ";")) <= as.numeric(substr(ii, 1, 4)))
  })
  n %v% "nyears" = as.integer(sp[ network.vertex.names(n), "nyears" ])

  set.edge.attribute(n, "source", as.character(edges[, 1])) # cosponsor
  set.edge.attribute(n, "target", as.character(edges[, 2])) # first author

  set.edge.attribute(n, "raw", edges$raw) # raw edge counts
  set.edge.attribute(n, "nfw", edges$nfw) # Newman-Fowler weights
  set.edge.attribute(n, "gsw", edges$gsw) # Gross-Shalizi weights

  # ============================================================================
  # SAVE PLOTS
  # ============================================================================

  if (plot) {

    save_plot(n, paste0("plots/net_lt", ii),
              i = colors[ sp[ n %e% "source", "party" ] ],
              j = colors[ sp[ n %e% "target", "party" ] ],
              mode, colors)

  }

  # ============================================================================
  # SAVE OBJECTS
  # ============================================================================

  assign(paste0("net_lt", substr(ii, 1, 4)), n)
  assign(paste0("edges_lt", substr(ii, 1, 4)), edges)
  assign(paste0("bills_lt", substr(ii, 1, 4)), bills)

  # ============================================================================
  # SAVE GEXF
  # ============================================================================

  if (gexf)
    save_gexf(n, paste0("net_lt", ii), mode, colors)

}

if (gexf)
  zip("net_lt.zip", dir(pattern = "^net_lt\\d{4}-\\d{4}\\.gexf$"))
