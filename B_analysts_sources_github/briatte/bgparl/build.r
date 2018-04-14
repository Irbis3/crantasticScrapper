years = c("2001-2005" = 39, "2005-2009" = 40, "2009-2013" = 41,
          "2013-2014" = 42, "2014-2018" = 43)

for (l in m$legislature %>% unique %>% sort) {
  
  data = filter(m, legislature == l & n_au > 1)
  cat("Legislature", l, ":", nrow(data), "cosponsored documents, ")
  
  sp = filter(s, legislature == years[ l ]) %>% data.frame
  rownames(sp) = sp$name
  
  # ============================================================================
  # DIRECTED EDGE LIST
  # ============================================================================
  
  edges = lapply(data$authors, function(i) {
    
    w = strsplit(i, ";") %>% unlist
    return(expand.grid(i = sp$name[ sp$url %in% w ],
                       j = sp$name[ sp$url == w[1]],
                       w = length(w) - 1,
                       stringsAsFactors = FALSE))
    
  }) %>% bind_rows
  
  # ============================================================================
  # EDGE WEIGHTS
  # ============================================================================
  
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
  n %n% "years" = l
  n %n% "legislature" = years[ l ] %>% as.character
  n %n% "chamber" = meta[ "ch" ] %>% as.character
  n %n% "type" = meta[ "type" ] %>% as.character
  n %n% "ipu" = meta[ "ipu" ] %>% as.integer
  n %n% "seats" = meta[ "seats" ] %>% as.integer
  
  n %n% "n_cosponsored" = nrow(data)
  n %n% "n_sponsors" = table(filter(m, legislature == l)$n_au)

  # ============================================================================
  # VERTEX-LEVEL ATTRIBUTES
  # ============================================================================

  n_au = as.vector(n_au[ network.vertex.names(n) ])

  n %v% "n_au" = ifelse(is.na(n_au), 0, n_au)
  
  n_co = as.vector(n_co[ network.vertex.names(n) ])
  n %v% "n_co" = ifelse(is.na(n_co), 0, n_co)
  
  n %v% "n_bills" = n %v% "n_au" + n %v% "n_co"
  
  cat(network.size(n), "nodes\n")
  
  n %v% "url" = paste0("http://www.parliament.bg/en/MP/",
                       sp[ network.vertex.names(n), "url" ])
  n %v% "sex" = sp[ network.vertex.names(n), "sex" ]
  n %v% "born" = sp[ network.vertex.names(n), "born" ]
  n %v% "party" = sp[ network.vertex.names(n), "party" ]
  n %v% "partyname" = groups[ n %v% "party" ] %>% as.character
  n %v% "lr" = scores[ n %v% "party" ] %>% as.numeric
  n %v% "photo" = sp[ network.vertex.names(n), "photo" ]
  n %v% "nyears" = sp[ network.vertex.names(n), "nyears" ] # pre-computed
  n %v% "constituency" = sp[ network.vertex.names(n), "constituency" ]
  
  set.edge.attribute(n, "source", as.character(edges[, 1])) # cosponsor
  set.edge.attribute(n, "target", as.character(edges[, 2])) # first author
  
  set.edge.attribute(n, "raw", edges$raw) # raw edge counts
  set.edge.attribute(n, "nfw", edges$nfw) # Newman-Fowler weights
  set.edge.attribute(n, "gsw", edges$gsw) # Gross-Shalizi weights
    
  # ============================================================================
  # SAVE PLOTS
  # ============================================================================
  
  if (plot) {
    
    save_plot(n, paste0("plots/net_bg", l),
              i = colors[ sp[ n %e% "source", "party" ] ],
              j = colors[ sp[ n %e% "target", "party" ] ],
              mode, colors)
    
  }
  
  # ============================================================================
  # SAVE OBJECTS
  # ============================================================================
  
  assign(paste0("net_bg", substr(l, 1, 4)), n)
  assign(paste0("edges_bg", substr(l, 1, 4)), edges)
  assign(paste0("bills_bg", substr(l, 1, 4)), data)
  
  # ============================================================================
  # SAVE GEXF
  # ============================================================================
    
  if (gexf)
    save_gexf(n, paste0("net_bg", l), mode, colors)
  
}

if (gexf)
  zip("net_bg.zip", dir(pattern = "^net_bg\\d{4}-\\d{4}\\.gexf$"))
