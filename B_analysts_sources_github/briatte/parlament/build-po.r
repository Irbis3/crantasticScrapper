for (l in rev(unique(b$legislature))) {

  cat("\nLegislature", l, "years", yrs[ as.character(l) ])

  data = filter(b, legislature == l & n_au > 1)

  cat(":", nrow(data), "cosponsored documents, ")

  # remove duplicated sponsors and chamber presidents, and add root URL
  data$authors = sapply(data$authors, function(x) {
    x = strsplit(x, ";") %>% unlist %>% unique
    x = x[ !grepl("o=7&id=5462|o=6&id=401", x) ]
    paste0(paste0(root, "", x), collapse = ";")
  })
  
  sp = filter(s, legislature == l)
  rownames(sp) = sp$name

  # check for missing sponsors
  a = strsplit(data$authors, ";") %>% unlist
  a = a[ !a %in% s$url ]
  if (length(a)) {
    cat("Missing", n_distinct(a), "sponsors:\n")
    print(table(a))
  }

  # ===========================================================================
  # DIRECTED EDGE LIST
  # ===========================================================================

  edges = lapply(data$authors, function(d) {

    w = strsplit(d, ";") %>% unlist
    return(expand.grid(i = sp$name[ sp$url %in% w ],
                       j = sp$name[ sp$url == w[1]],
                       w = length(w) - 1, # number of cosponsors
                       stringsAsFactors = FALSE))

  }) %>% bind_rows

  # ===========================================================================
  # EDGE WEIGHTS
  # ===========================================================================

  # first author self-loops, with counts of cosponsors
  self = filter(edges, i == j)

  # count number of bills per first author
  n_au = table(self$j)

  # remove self-loops from directed edge list
  edges = filter(edges, i != j)

  # count number of bills cosponsored per sponsor
  n_co = table(edges$i)

  # identify directed ties
  edges$ij = apply(edges[, 1:2 ], 1, paste0, collapse = "///")

  # raw edge counts
  raw = table(edges$ij)

  # Newman-Fowler weights (weighted quantity of bills cosponsored)
  edges = aggregate(w ~ ij, function(x) sum(1 / x), data = edges)

  # expand to edge list
  edges = data.frame(i = gsub("(.*)///(.*)", "\\1", edges$ij),
                     j = gsub("(.*)///(.*)", "\\2", edges$ij),
                     raw = as.vector(raw[ edges$ij ]), # raw edge counts
                     nfw = edges$w, stringsAsFactors = FALSE)

  # Gross-Shalizi weights (weighted propensity to cosponsor)
  edges = merge(edges, aggregate(w ~ j, function(x) sum(1 / x), data = self))
  edges$gsw = edges$nfw / edges$w

  # sanity check
  stopifnot(edges$gsw <= 1)

  # final edge set: cosponsor, first author, weights
  edges = select(edges, i, j, raw, nfw, gsw)

  cat(nrow(edges), "edges, ")

  # ===========================================================================
  # DIRECTED NETWORK
  # ===========================================================================

  n = network(edges[, 1:2 ], directed = TRUE)

  n %n% "country" = meta[ "cty" ] %>% as.character
  n %n% "lang" = meta[ "lang" ] %>% as.character
  n %n% "years" = yrs[ as.character(l) ] %>% as.character
  n %n% "legislature" = l %>% as.character
  n %n% "chamber" = meta[ "ch-po" ] %>% as.character
  n %n% "type" = meta[ "type-po" ] %>% as.character
  n %n% "ipu" = meta[ "ipu-po" ] %>% as.integer
  n %n% "seats" = meta[ "seats-po" ] %>% as.integer

  n %n% "n_cosponsored" = nrow(data)
  n %n% "n_sponsors" = table(filter(b, legislature == l)$n_au)

  # ===========================================================================
  # VERTEX-LEVEL ATTRIBUTES
  # ===========================================================================

  n_au = as.vector(n_au[ network.vertex.names(n) ])

  n %v% "n_au" = ifelse(is.na(n_au), 0, n_au)

  n_co = as.vector(n_co[ network.vertex.names(n) ])
  n %v% "n_co" = ifelse(is.na(n_co), 0, n_co)

  n %v% "n_bills" = n %v% "n_au" + n %v% "n_co"

  cat(network.size(n), "nodes\n")

  rownames(s) = s$uid
  n %v% "url" = sp[ network.vertex.names(n), "url" ]
  n %v% "sex" = sp[ network.vertex.names(n), "sex" ]
  n %v% "born" = sp[ network.vertex.names(n), "born" ]
  n %v% "party" = sp[ network.vertex.names(n), "party" ]
  n %v% "partyname" = groups[ n %v% "party" ] %>% as.character
  n %v% "lr" = scores[ n %v% "party" ] %>% as.numeric

  n %v% "nyears" = sp[ network.vertex.names(n), "mandate" ]
  n %v% "nyears" = sapply(n %v% "nyears", function(x) {

    x = strsplit(x, ";") %>% unlist
    x = yrs[ x[ x < l ] ]
    
    if (!length(x)) {
      
      # no record in earlier legislatures
      return(0)
      
    } else {
      
      # seniority since legislature 1, in years
      x = lapply(x, function(x) strsplit(x, "-") %>% unlist %>% as.integer) # x-y
      x = sapply(x, function(x) seq(min(x), max(x)) %>% length) # sequence length
      return(sum(x))
      
    }
    
  }) %>% as.integer
  n %v% "constituency" = as.character(sp[ network.vertex.names(n), "constituency" ])
  n %v% "photo" = sp[ network.vertex.names(n), "photo" ]

  set.edge.attribute(n, "source", as.character(edges[, 1])) # cosponsor
  set.edge.attribute(n, "target", as.character(edges[, 2])) # first author

  set.edge.attribute(n, "raw", edges$raw) # raw edge counts
  set.edge.attribute(n, "nfw", edges$nfw) # Newman-Fowler weights
  set.edge.attribute(n, "gsw", edges$gsw) # Gross-Shalizi weights

  # ===========================================================================
  # SAVE PLOTS
  # ===========================================================================

  if (plot) {

    save_plot(n, paste0("plots/net_cz_po", yrs[ as.character(l) ]),
              i = colors[ sp[ n %e% "source", "party" ] ],
              j = colors[ sp[ n %e% "target", "party" ] ],
              mode, colors)

  }

  # ===========================================================================
  # SAVE OBJECTS
  # ===========================================================================

  assign(paste0("bills_cz_po", substr(yrs[ as.character(l) ], 1, 4)), data)
  assign(paste0("edges_cz_po", substr(yrs[ as.character(l) ], 1, 4)), edges)
  assign(paste0("net_cz_po", substr(yrs[ as.character(l) ], 1, 4)), n)

  # ===========================================================================
  # SAVE GEXF
  # ===========================================================================

  if (gexf)
    save_gexf(n, paste0("net_cz_po", yrs[ as.character(l) ]), mode, colors)

}

if (gexf)
  zip("net_cz_po.zip", dir(pattern = "^net_cz_po\\d{4}-\\d{4}\\.gexf$"))
