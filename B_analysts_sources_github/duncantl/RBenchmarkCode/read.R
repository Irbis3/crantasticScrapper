readTrace = 
function(f = "offline.final.trace.txt", txt = readLines(f, n = n)[-(1:3)], n = -1)
{
    # remove the comment lines
  txt = txt[ - grep("^#", txt) ]
  txt = txt[ txt != ""]

    # break the lines into the primary components separated by ;
  els = strsplit(txt, ";")
    # find out how many signal strengths we have for each record, i.e the number of responders/receivers.
  numMacs = sapply(els, length) - 4

    # now grab the signal strengths and from who
  macs = lapply(els, function(x) {
                            macs = x[-(1:4)]   # drop the first four elements
                            if(length(macs) == 0)
                               return(matrix(NA, 0, 4)) 
                            matrix(unlist(strsplit(macs, "(=|,)")), , 4, byrow = TRUE)
                          })

  
#  macs = macs[numMacs > 0]
#  numMacs = numMacs[numMacs > 0]

  time = sapply(els, function(x) strsplit(x[1], "=")[[1]][2])
  id = sapply(els, function(x) strsplit(x[2], "=")[[1]][2])
  pos = sapply(els, function(x) { strsplit(gsub("pos=", "", x[3]), ",")[[1]]})
  
  ans = as.data.frame(do.call("rbind", macs))
  names(ans) = c("mac", "signal", "channelFrequency", "mode")  
  ans$time = rep(time, numMacs)
  ans$id = rep(id, numMacs)
  ans$x = rep(as.numeric(pos[1,]), numMacs)
  ans$y = rep(as.numeric(pos[2,]), numMacs)
  ans$orientation = rep(as.numeric(gsub("degree=", "", sapply(els, function(x) x[4]))), numMacs)  

  ans$signal = as.numeric(as.character(ans$signal))
  ans$channelFrequency = as.numeric(as.character(ans$channelFrequency) )
  ans$orientationLevel = roundOrientation(ans$orientation)
  
  tmp = c("adhoc" = "1", "access_point" = "3")
  levels(ans$mode) = names(tmp)[ match(levels(ans$mode), tmp) ]


  if(FALSE) {
     #data.frame(time = time, id = id, x = pos[1,], y = pos[2,], z = pos[3,],
     #           orientation = as.numeric(gsub("degree=", "", sapply(els, function(x) x[4]))),
     #             numResponers = numMacs,
     #           macs = I(macs))
  }
  
  ans
}


 # These are eyeballed off the map
 #  should be able to get from the paper or from a ruler and the map
 # or by back projecting from 
AP =
matrix(c(1, 14,
         2.5, -.8, 
         7.5, 6.5,
         13, -2.8,
          33.5, 2.8,
          33.5, 8.3), , 2, byrow = TRUE,
          dimnames = list(c("00:14:bf:b1:97:90",
                            "00:14:bf:b1:97:8a",
                             "00:0f:a3:39:e1:c0",
                             "00:14:bf:3b:c7:c6",
                             "00:14:bf:b1:97:81",
                             "00:14:bf:b1:97:8d" ),
                           c("x", "y")))

# Lookup Vendor-Mac prefix mapping at
#   http://www.coffer.com/mac_find
# There is no Lancom

# is 97:8a at the top left?


roundOrientation = 
function(x)
{
  refs = seq(0, by = 45, length  = 9)
  q = sapply(x, function(o) which.min(abs(o - refs)))
  c(refs[1:8], 0)[q]
}


if(FALSE) {
 off = readTrace("~/NetworkData/mannheim/offline.final.trace.txt")
 on = readTrace("~/NetworkData/mannheim/online.final.trace.txt")

      # plot the information with the offline and online data points and the access
      # points.
 plot(off$x, off$y, xlim = range(c(off$x, AP[,1])), ylim = range(c(off$y, AP[,2])))
 points(AP, pch = rownames(AP), fg = "red", cex = 2, col = "red")
 symbols(on$x, on$y, circles = rep(.1, length(on$x)), bg = "blue", add = TRUE, inches = FALSE)
}
