readEntireLog =
  # This reads in the entire log w/o filtering based on the interface type.
  # It arranges the
  # file - the name of the log file
  # tt - the lines from the file in case one wants to pass a previously read file
  #    or a subset.
  #
  # This returns a character matrix with the individual values for each row
  # with columns for each of the common elements, e.g. time, host, robot, interface, index, type,
  #  subtype,  but with the variable part as the payload as a single string.
  #
  #  robot and host are unique
  #  http://playerstage.sourceforge.net/wiki/Data_logging
  #  http://playerstage.sourceforge.net/doc/Player-svn/player/group__tutorial__datalog.html
function(file = "logs/JRSPdata_2010_03_10_12_12_31.log",
         tt = readLines(file))
{
  tt = grep("^#", tt, invert = TRUE, value = TRUE)
  els = strsplit(tt, "[[:space:]]+")
  tmp = do.call(rbind, lapply(els, `[`, 1:7))  
  tmp = cbind(tmp,  sapply(els, function(x) paste(x[-(1:7)], collapse = " ")))
  colnames(tmp) = c("time", "host", "robot", "interface", "index", "type", "subtype", "payload")
  tmp
}


readLog =
function(file = "logs/JRSPdata_2010_03_10_12_12_31.log",
         log = readEntireLog(file))
{
   i = log[, "interface"] %in% c("position2d", "laser") & log[, "type"] == "001"
   d = log[i,]
   
   d = d[, c("time", "interface", "type", "payload")]
   i = which(d[, "interface"] == "position2d")
     # there are some position2d's followed by another position2d
   i = i[d[i+1, "interface"] == "laser"]
   pos = d[i, 1:3]
   pos = data.frame(time = as.numeric(pos[, "time"]), interface = pos[, "interface"])

   xy = t(sapply(strsplit(d[i, "payload"], " "), getXY))
   colnames(xy) = c("x", "y")
   
    # now get the payload from the i+1 laser elements
   laser = lapply(d[i+1, "payload"], getLaserPayload)
   laser = do.call(rbind, laser)
   colnames(laser) = sprintf("Range_%d", 1:ncol(laser))

   df = cbind(pos, as.data.frame(xy), laser)

   invisible(structure(df, class = c("RobotLog", "data.frame")))
}

getXY =
function(els)
{
  as.numeric(els[c(1, 2)])
}

getLaserPayload =
function(str)
{
  els = strsplit(str, " ")[[1]]
  vals = els[-(1:6)]
  as.numeric(vals[-seq(2, length(vals), by = 2) ])
}

plot.RobotLog =
function(x, y, col = makeColorRamp(nrow(x)), ...)
{
 plot(y ~ x,  x, type = "p", pch = 20, col = col, ...)
 points(x$x[c(1, nrow(x))],  x$y[c(1, nrow(x))],
          pch = c("O", "+"), col = c("green", "red"))
}

makeColorRamp =
function(n)
{
  s = (1:n)/n
  zero = rep(0, n)
  rgb(zero, s, zero)
}


if(FALSE) {
  plot(density(unlist(dd[, -(1:4)])))

  ff = list.files("logs", pattern = "log$", full = TRUE)
  logs = lapply(ff, readLog)  
}

# testing
if(FALSE) {
  par(mfrow = c(3, 3))
  si = sample(seq(along = logs), 9)
  sapply(logs[si], plot.RobotLog)
}

if(FALSE) {
 par(mfrow = c(4, 5), mar = c(0, 0, 0, 0))  
 sapply(logs[21:40], showFinalLook, axes = FALSE)
}

checkMove = function(log) 
  c(x = max(diff(log$x)), y =    max(diff(log$y)))

checkVelocity = function(log) 
  c(x = max(diff(log$x)/diff(log$time)), y = max(diff(log$y)/diff(log$time)))

summaryVelocity = function(log) 
  rbind(x = summary(diff(log$x)/diff(log$time)), y = summary(diff(log$y)/diff(log$time)))


if(FALSE) {
 # Compute change in x's and y's to see if any jump to big.


}
