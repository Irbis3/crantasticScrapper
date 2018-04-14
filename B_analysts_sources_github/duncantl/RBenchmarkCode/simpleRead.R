read.log =
function(filename = "Example.log")
{  

   ll =  readLines(filename)

   ll = ll[! grepl("^#", ll) ]

# Previously, got lines for  based on the interface and
# subsequently filtered on the type being 001
# But can do it in a single    
#   ll = grep("(position2d|laser)", ll, value = TRUE)
   ll = grep("(position2d|laser) [0-9]+ 001", ll, value = TRUE)   

   # is "[[:space:]]+" is faster or slower than " "
   els = strsplit(ll, "[[:space:]]+")   

#
#   type = sapply(els, `[`, 6)
#   els = els[type == "001"]
   
   basic = do.call(rbind, lapply(els, `[`, 1:7))
         # col names come from the header comment
   colnames(basic) = c("time", "host", "robot", "interface", "index", "type", "subtype")
   payload = lapply(els, `[`, -(1:7))
   list(basic = basic, payload = payload)
}

# host and robot should be a unique identifier in each log file
# index is unique
# so to is type, by filtering.
# interface will be just position2d or laser     

read.log.scan =
function(filename = "Example.log")
{  

   ll =  readLines(filename)

   ll = ll[! grepl("^#", ll) ]

# Previously, got lines for  based on the interface and
# subsequently filtered on the type being 001
# But can do it in a single    
#   ll = grep("(position2d|laser)", ll, value = TRUE)
   ll = grep("(position2d|laser) [0-9]+ 001", ll, value = TRUE)   


   pos2d = scan(textConnection(grep("position2d", ll, value = TRUE)), what = "")
   laser = scan(textConnection(grep("laser", ll, value = TRUE)), what = "")
   return(list(pos2d = pos2d, laser = laser))

#
#   type = sapply(els, `[`, 6)
#   els = els[type == "001"]
   
   basic = do.call(rbind, lapply(els, `[`, 1:7))
   payload = lapply(els, `[`, -(1:7))
   list(basic = basic, payload = payload)
}
     
