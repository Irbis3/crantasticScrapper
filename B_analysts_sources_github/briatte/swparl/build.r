for (jj in unique(b$chamber)) {
  
  for (ii in unique(b$legislature) %>% sort) {
    
    cat("\nChamber", jj, "years", ii)
    data = subset(b, chamber == jj & legislature == ii & n_au > 1)
    sp = subset(s, chamber == jj & legislature == ii) %>% data.frame
    
    cat(":", nrow(data), "cosponsored documents, ")
    
    # ==========================================================================
    # DIRECTED EDGE LIST
    # ==========================================================================
    
    edges = bind_rows(lapply(data$sponsors, function(d) {
      
      w = unlist(strsplit(d, ";"))
      
      d = expand.grid(i = sp$name[ sp$id %in% w ],
                      j = sp$name[ sp$id == w[1]], stringsAsFactors = FALSE)
      
      return(data.frame(d, w = length(w) - 1)) # number of cosponsors
      
    }))
    
    # ==========================================================================
    # EDGE WEIGHTS
    # ==========================================================================
    
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
    edges = edges[, c("i", "j", "raw", "nfw", "gsw") ]
    
    cat(nrow(edges), "edges, ")
    
    # ==========================================================================
    # DIRECTED NETWORK
    # ==========================================================================
    
    n = network(edges[, 1:2 ], directed = TRUE)
    
    n %n% "country" = meta[ "cty" ] %>% as.character
    n %n% "lang" = meta[ "lang" ] %>% as.character
    n %n% "years" = ii %>% as.character
    n %n% "legislature" = c("1995-1999" = "45",
                            "1999-2003" = "46",
                            "2003-2007" = "47",
                            "2007-2011" = "48",
                            "2011-2015" = "49")[ ii ]
    n %n% "chamber" = meta[ jj ] %>% as.character
    n %n% "type" = meta[ paste0("type-", jj) ] %>% as.character
    n %n% "ipu" = meta[ paste0("ipu-", jj) ] %>% as.integer
    n %n% "seats" = meta[ paste0("seats-", jj) ] %>% as.integer
    
    n %n% "n_cosponsored" = nrow(data)
    n %n% "n_sponsors" = table(subset(b, chamber == jj & legislature == ii)$n_au)

    # ==========================================================================
    # VERTEX-LEVEL ATTRIBUTES
    # ==========================================================================

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
    n %v% "lr" = scores[ n %v% "party" ] %>% as.numeric
    n %v% "constituency" = sp[ network.vertex.names(n), "constituency" ]
    n %v% "nyears" = sp[ network.vertex.names(n), "nyears" ]
    n %v% "photo" = sp[ network.vertex.names(n), "photo" ]
    
    set.edge.attribute(n, "source", as.character(edges[, 1]))
    set.edge.attribute(n, "target", as.character(edges[, 2]))
    
    set.edge.attribute(n, "source", as.character(edges[, 1])) # cosponsor
    set.edge.attribute(n, "target", as.character(edges[, 2])) # first author
    
    set.edge.attribute(n, "raw", edges$raw) # raw edge counts
    set.edge.attribute(n, "nfw", edges$nfw) # Newman-Fowler weights
    set.edge.attribute(n, "gsw", edges$gsw) # Gross-Shalizi weights
    
    # ==========================================================================
    # SAVE PLOTS
    # ==========================================================================
    
    if (plot) {
      
      save_plot(n, paste0("plots/net_ch_", jj, ii),
                i = colors[ sp[ n %e% "source", "party" ] ],
                j = colors[ sp[ n %e% "target", "party" ] ],
                mode, colors)
      
    }
    
    # ==========================================================================
    # SAVE OBJECTS
    # ==========================================================================
    
    assign(paste0("net_ch_",  jj, substr(ii, 1, 4)), n)
    assign(paste0("edges_ch_", jj, substr(ii, 1, 4)), edges)
    assign(paste0("bills_ch_", jj, substr(ii, 1, 4)), data)
    
    # ==========================================================================
    # SAVE GEXF
    # ==========================================================================
    
    if (gexf)
      save_gexf(n, paste0("net_ch_", jj, ii), mode, colors)
    
  }
  
  if (gexf)
    zip(paste0("net_ch_", jj, ".zip"), dir(pattern = paste0("^net_ch_", jj, "\\d{4}-\\d{4}\\.gexf$")))
  
}
