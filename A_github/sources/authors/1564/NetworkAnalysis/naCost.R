naCost <-
function(G){  
    out <- ecount(G)/(vcount(G)*(vcount(G)-1)/2)
return(out)
}

