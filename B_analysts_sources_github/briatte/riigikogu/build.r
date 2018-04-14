years = c(
  "11" = "2007-2011",
  "12" = "2011-2015",
  "13" = "2015-2018"
)

# remove government bills
b = filter(b, !grepl("Vabariigi Valitsus", authors))
b$n_a[ !b$n_a ] = 1

for (ii in 11:13) {

  cat("\nLegislature", ii)
  data = subset(b, legislature == ii & n_a > 1)
  sp = subset(s, legislature == ii) %>% data.frame

  cat(":", nrow(data), "cosponsored documents, ")

  # check for missing sponsors
  u = strsplit(data$authors, ";") %>% unlist
  u = na.omit(u[ !u %in% s$name ])

  cat(n_distinct(u[ !grepl("fraktsioon|komisjon", u) ]), "missing sponsor(s), ")
  data$authors = sapply(data$authors, function(d) {
    w = strsplit(d, ";") %>% unlist
    paste0(sp$name[ sp$name %in% w ], collapse = ";")
  })

  # ============================================================================
  # DIRECTED EDGE LIST
  # ============================================================================

  edges = lapply(data$authors[ nchar(data$authors) > 0 ], function(d) {

    w = strsplit(d, ";") %>% unlist
    d = expand.grid(i = sp$name[ sp$name %in% w ],
                    j = sp$name[ sp$name == w[1]], stringsAsFactors = FALSE)

    return(data.frame(d, w = length(w) - 1)) # number of cosponsors

  }) %>% bind_rows

  # ============================================================================
  # EDGE WEIGHTS
  # ============================================================================

  # first author self-loops, with counts of cosponsors
  self = subset(edges, i == j)

  # count number of bills per first author
  n_a = table(self$j)

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
  n %n% "years" = years[ as.character(ii) ]
  n %n% "legislature" = ii
  n %n% "chamber" = meta[ "ch" ] %>% as.character
  n %n% "type" = meta[ "type" ] %>% as.character
  n %n% "ipu" = meta[ "ipu" ] %>% as.integer
  n %n% "seats" = meta[ "seats" ] %>% as.integer

  n %n% "n_cosponsored" = nrow(data)
  n %n% "n_sponsors" = table(subset(b, legislature == ii)$n_a)

  # ============================================================================
  # VERTEX-LEVEL ATTRIBUTES
  # ============================================================================

  n_a = as.vector(n_a[ network.vertex.names(n) ])

  n %v% "n_a" = ifelse(is.na(n_a), 0, n_a)

  n_co = as.vector(n_co[ network.vertex.names(n) ])
  n %v% "n_co" = ifelse(is.na(n_co), 0, n_co)

  n %v% "n_bills" = n %v% "n_a" + n %v% "n_co"

  cat(network.size(n), "nodes\n")

  rownames(sp) = sp$name
  n %v% "url" = sp[ network.vertex.names(n), "url" ]
  n %v% "sex" = sp[ network.vertex.names(n), "sex" ]
  n %v% "born" = sp[ network.vertex.names(n), "born" ]
  n %v% "party" = sp[ network.vertex.names(n), "party" ]
  n %v% "partyname" = groups[ n %v% "party" ] %>% as.character
  n %v% "lr" = scores[ n %v% "party" ] %>% as.numeric
  n %v% "nyears" = sp[ network.vertex.names(n), "nyears" ] %>% as.integer
  n %v% "constituency" = sp[ network.vertex.names(n), "constituency" ]
  n %v% "photo" = sp[ network.vertex.names(n), "photo" ]

  set.edge.attribute(n, "source", as.character(edges[, 1])) # cosponsor
  set.edge.attribute(n, "target", as.character(edges[, 2])) # first author

  set.edge.attribute(n, "raw", edges$raw) # raw edge counts
  set.edge.attribute(n, "nfw", edges$nfw) # Newman-Fowler weights
  set.edge.attribute(n, "gsw", edges$gsw) # Gross-Shalizi weights

  # ============================================================================
  # SAVE PLOTS
  # ============================================================================

  if (plot) {

    save_plot(n, paste0("plots/net_ee", years[ as.character(ii) ]),
              i = colors[ sp[ n %e% "source", "party" ] ],
              j = colors[ sp[ n %e% "target", "party" ] ],
              mode, colors)

  }

  # ============================================================================
  # SAVE OBJECTS
  # ============================================================================

  assign(paste0("net_ee", substr(years[ as.character(ii) ], 1, 4)), n)
  assign(paste0("edges_ee", substr(years[ as.character(ii) ], 1, 4)), edges)
  assign(paste0("bills_ee", substr(years[ as.character(ii) ], 1, 4)), data)

  # ============================================================================
  # SAVE GEXF
  # ============================================================================

  if (gexf)
    save_gexf(n, paste0("net_ee", years[ as.character(ii) ]), mode, colors)

}

if (gexf)
  zip("net_ee.zip", dir(pattern = "^net_ee\\d{4}-\\d{4}\\.gexf$"))
