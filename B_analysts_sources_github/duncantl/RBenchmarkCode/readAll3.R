readLog =
  #
  # Read the log file with only one strsplit.
  #
function(file = "logs/JRSPdata_2010_03_10_12_12_31.log")
{
  ll = readLines(file)
  i = grep("laser|position2d", ll)
  ll = ll[i]
  
  els = strsplit(ll, " +") # faster than "( |  )"
    # Get the interface and type so we can subset.
  iface = sapply(els, `[`, 4)
  type = sapply(els, `[`, 6)

    # find the indices corresponding to a position2d with a laser immediately after.
  i = which(iface == "position2d" & type == "001")
  i = i[ iface[i+1] == "laser" & type[i+1] == "001"]

    # Get the time, x, y, and then the range values from the laser below.      
  locations = t(sapply(els[i], function(x) as.numeric( x[c(1, 9, 10)])))
  ranges = t(sapply(els[i + 1], function(x) as.numeric(x[ seq(14, by = 2, length = 361) ])))

    # now combine these into a data frame
  ans = cbind(locations, ranges)
#  ans = matrix(str, nrow(str), ncol(str), byrow = TRUE)
  colnames(ans) = c("time", "x", "y", sprintf("range%d", 1:ncol(ranges)))
  invisible(ans)
}
