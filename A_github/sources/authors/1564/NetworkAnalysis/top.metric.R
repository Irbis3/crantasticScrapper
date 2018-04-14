top.metric <-
function(G,metric="ge"){

    if(metric=="ge")   out <- ge(G)
    if(metric=="le")   out <- le(G)
      
return(out)
}# top.metric

