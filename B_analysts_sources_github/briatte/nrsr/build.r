legislatures = c(
  "1" = "1994-1998",
  "2" = "1998-2002",
  "3" = "2002-2006",
  "4" = "2006-2010",
  "5" = "2010-2012",
  "6" = "2012-2016"
)

for (ii in a$legislature %>% unique %>% sort) {

  cat("\nLegislature", ii, "years", legislatures[ ii ])
  data = subset(a, legislature == ii & n_au > 1)

  sp = subset(s, legislature == ii) %>% data.frame
  stopifnot(!duplicated(sp$uid))

  cat(":", nrow(data), "cosponsored documents, ")

  # ============================================================================
  # DIRECTED EDGE LIST
  # ============================================================================

  edges = lapply(data$authors, function(d) {

    w = unlist(strsplit(d, ", "))

    d = expand.grid(i = sp$uid[ sp$uid %in% w  ],
                    j = sp$uid[ sp$uid == w[1] ], stringsAsFactors = FALSE)

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
  n %n% "years" = legislatures[ as.character(ii) ] %>% as.character
  n %n% "legislature" = ii %>% as.character
  n %n% "chamber" = meta[ "ch" ] %>% as.character
  n %n% "type" = meta[ "type" ] %>% as.character
  n %n% "ipu" = meta[ "ipu" ] %>% as.integer
  n %n% "seats" = meta[ "seats" ] %>% as.integer

  n %n% "n_cosponsored" = nrow(data)
  n %n% "n_sponsors" = table(subset(a, legislature == ii)$n_au)

  # ============================================================================
  # VERTEX-LEVEL ATTRIBUTES
  # ============================================================================

  n_au = as.vector(n_au[ network.vertex.names(n) ])

  n %v% "n_au" = ifelse(is.na(n_au), 0, n_au)

  n_co = as.vector(n_co[ network.vertex.names(n) ])
  n %v% "n_co" = ifelse(is.na(n_co), 0, n_co)

  n %v% "n_bills" = n %v% "n_au" + n %v% "n_co"

  cat(network.size(n), "nodes\n")

  rownames(sp) = sp$uid
  n %v% "url" = sp[ network.vertex.names(n), "url" ]
  n %v% "sex" = sp[ network.vertex.names(n), "sex" ]
  n %v% "born" = sp[ network.vertex.names(n), "born" ]
  n %v% "party" = sp[ network.vertex.names(n), "party" ]
  n %v% "partyname" = groups[ n %v% "party" ] %>% as.character
  n %v% "lr" = scores[ n %v% "party" ] %>% as.numeric
  n %v% "constituency" = sp[ network.vertex.names(n), "county" ]
  n %v% "nyears" = sp[ network.vertex.names(n), "nyears" ]
  n %v% "photo" = sp[ network.vertex.names(n), "photo" ]

  set.edge.attribute(n, "source", as.character(edges[, 1]))
  set.edge.attribute(n, "target", as.character(edges[, 2]))

  set.edge.attribute(n, "source", as.character(edges[, 1])) # cosponsor
  set.edge.attribute(n, "target", as.character(edges[, 2])) # first author

  set.edge.attribute(n, "raw", edges$raw) # raw edge counts
  set.edge.attribute(n, "nfw", edges$nfw) # Newman-Fowler weights
  set.edge.attribute(n, "gsw", edges$gsw) # Gross-Shalizi weights

  # ============================================================================
  # SAVE PLOTS
  # ============================================================================

  if (plot) {

    save_plot(n, paste0("plots/net_sk", legislatures[ ii ]),
              i = colors[ sp[ n %e% "source", "party" ] ],
              j = colors[ sp[ n %e% "target", "party" ] ],
              mode, colors)

  }

  # ============================================================================
  # SAVE OBJECTS
  # ============================================================================

  # replace uids with names
  network.vertex.names(n) = sp[ network.vertex.names(n), "name" ]

  set.edge.attribute(n, "source", sp[ n %e% "source", "name" ])
  set.edge.attribute(n, "target", sp[ n %e% "target", "name" ])

  edges$i = sp[ edges$i, "name" ]
  edges$j = sp[ edges$j, "name" ]

  # check the names are unique
  u = network.vertex.names(n)[ duplicated(network.vertex.names(n)) ]
  stopifnot(!length(u))

  assign(paste0("net_sk", substr(legislatures[ ii ], 1, 4)), n)
  assign(paste0("edges_sk", substr(legislatures[ ii ], 1, 4)), edges)
  assign(paste0("bills_sk", substr(legislatures[ ii ], 1, 4)), data)

  # ============================================================================
  # SAVE GEXF
  # ============================================================================

  if (gexf)
    save_gexf(n, paste0("net_sk", legislatures[ ii ]), mode, colors)

}

if (gexf)
  zip("net_sk.zip", dir(pattern = "^net_sk\\d{4}-\\d{4}\\.gexf$"))
