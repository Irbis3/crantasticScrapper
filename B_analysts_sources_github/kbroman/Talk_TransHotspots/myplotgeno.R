source("color.R")
hgrey <- "grey90"
hcolor <- "yellow"
hlwd <- 2
nlay <- 25

addqtl <- function(qtlloc,qtlgn){
  abline(h=qtlloc,col="yellow",lwd=2)
  points(1:n, rep(qtlloc, n), cex=1.4, pch=23, col="black", bg=color[qtlgn])
  axis(side=4, tick=FALSE, qtlloc, "QTL", las=1, mgp=c(0, 0.5, 0), cex.axis=1.8)
}

addgn <- function(g){
  ## draw genotypes
  for(i in 1:nrow(g)) {
    wh <- !is.na(g[i,])
    points(rep(i, sum(wh)), map[wh], bg=color[g[i,wh]], col="black", pch=21, cex=1.2)
  }
}

myplotgeno <- function(map,g, mar=c(2.6, 7.6, 3.1, 4.1), mgp=c(1.1,0, 0),
                       axislab, axislabadj)
{
    if(missing(axislab)) axislab <- seq(along=map)
    if(missing(axislabadj)) axislabadj <- rep(0, length(axislab))

  p <- length(map)
  n <- nrow(g)

  par(col.lab="slateblue", mar=mar)
  plot(rep(0, p), map, type="n", mgp=mgp,
       xlim=c(0.2, n+0.8), xaxs="i",
       ylim=c(map[p], map[1]),
       ylab="", xlab="Mouse", yaxt="n",xaxt="n",cex.lab=1.5)
  title(ylab="Position (Mbp)", mgp=c(6.1, 0, 0),cex.lab=1.5)

  map.p <- map[axislab]
  axis(side=2, tick=FALSE, map.p+axislabadj, sprintf("%.2f",map.p), las=1, mgp=c(0, 0.2, 0), cex.axis=1.8)

  u <- par("usr")
  rect(u[1], u[3], u[2], u[4],col="grey80", border=TRUE)
  abline(h=map,col="white")

#  x <- u[1] + diff(u[1:2])*c(0.4, 0.5, 0.6)
#  points(x, rep(u[4]+diff(u[3:4])*0.035, 3), pch=21, col="black",
#         bg=color, xpd=TRUE, cex=1.5)
#  x <- u[1] + diff(u[1:2])*(c(0.4, 0.5, 0.6)+0.04)
#  text(x, rep(u[4]+diff(u[3:4])*0.039, 3), c("BB","BR","RR"),
#       col=color, xpd=TRUE, cex=1.2)

  xocolor <- "#aaaaaa"

  ## draw cross-overs
  for(i in 1:n){
    for(j in 1:(p-1)){
      if(!is.na(g[i,j])){
        for(k in (j+1):p){
          if(!is.na(g[i,k])){
            if(g[i,j] == g[i,k]){
              segments(i,map[j],i,map[k],col=color[g[i,j]],lwd=3)
            }else{
              segments(i,map[j],i,map[k],col=xocolor,lwd=3)
            }
            break
          }
        }
      }
    }
  }

  i <- 1:n
  segments(i,map[p],i,u[3],col=color[g[i,p]],lwd=3)
  segments(i,map[1],i,u[4],col=color[g[i,1]],lwd=3)

  ## draw genotypes
  for(i in 1:nrow(g)) {
    wh <- !is.na(g[i,])
    points(rep(i, sum(wh)), map[wh], bg=color[g[i,wh]], col="black", pch=21, cex=1.2)
  }
  invisible()
}


myplotgeno.eh <- function(map,g, rect)
{
  p <- length(map)
  n <- nrow(g)

  u <- par("usr")
  abline(h=map,col="white")

  xocolor <- "#cccccc"

  ## draw cross-overs
  for(i in 1:n){
    for(j in 1:(p-1)){
      if(!is.na(g[i,j])){
        for(k in (j+1):p){
          if(!is.na(g[i,k])){
            if(g[i,j] == g[i,k]){
              segments(i,map[j],i,map[k],col=color[g[i,j]],lwd=3)
          }else{
              if(i >= rect[1] && i <= rect[3]
                 && map[j] >= rect[2] && map[k] <= rect[4])
                  segments(i,map[j],i,map[k],col=xocolor,lwd=3)
            }
            break
          }
        }
      }
    }
  }

  i <- 1:n
  segments(i,map[p],i,u[3],col=color[g[i,p]],lwd=3)
  segments(i,map[1],i,u[4],col=color[g[i,1]],lwd=3)

  ## draw genotypes
  for(i in 1:nrow(g)) {
    wh <- !is.na(g[i,])
    points(rep(i, sum(wh)), map[wh], bg=color[g[i,wh]], col="black", pch=21, cex=1.2)
  }

  invisible()
}
