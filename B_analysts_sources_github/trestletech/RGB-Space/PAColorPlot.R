col <- read.csv("../ColorPalettes.csv", stringsAsFactors=FALSE)
sequential <- col[1:756,]
diverging <- col[757:1323,]
qualitative <- col[1324:1689,]

#scatterplot3d(cols$R, cols$G, cols$B, type="h", color=cols[,4], pch=19)

library(rgl)

propVar <- function(pca, components=1){
  #proportion of variance explained by the first component
  pv <- pca$sdev^2/sum(pca$sdev^2)[1] 
  return(sum(pv[1:components]))
}



plotCols <- function(red,green,blue,pca=0, col=rgb(red/255, green/255, blue/255), ...){  
  
  #if on a [0,1] interval, shift to [0,255] interval
  if (max(red, blue,green) < 1){
    red <- red * 255
    blue <- blue * 255
    green <- green * 255
  }
  
  plot3d(red, green, blue,          
         xlab="Red", ylab="Green", zlab="Blue", 
         type="l", lwd=2, col=col, xlim=c(0,255), ylim=c(0,255), zlim=c(0,255),...)
  
  plot3d(red, green, blue,           
         type="p", add=TRUE, pch=19, size=6, col=col, ...)
  
  X <- cbind(red,green,blue)
  #from Hans Borchers, http://r.789695.n4.nabble.com/Fit-a-3-Dimensional-Line-to-Data-Points-td863596.html
  N <- nrow(X) 
  
  meanX <- apply(X, 2, mean) 
  Xpca   <- princomp(X) 
  
  if (pca==TRUE){
    pca <- 1
  }
  
  endpts <- list()
  for (pc in 1:pca){    
    dirVector <- Xpca$loadings[, pc] 
    t <- c(min(Xpca$score[, pc])-.2, max(Xpca$score[, pc])+.2) 
    endpts[[pc]] <- rbind(meanX + t[1]*dirVector, meanX + t[2]*dirVector)     
  }
  
  if (pca){
    for (pc in 1:pca){  
      plot3d(endpts[[pc]][,1], endpts[[pc]][,2], endpts[[pc]][,3], type="l", add=TRUE, aspect=FALSE)
    }
    
    pv <- propVar(Xpca, pca)
    text3d(50,50,250, paste("R2 = ", round(pv,4), sep=""), aspect=FALSE)
  }
  
  view3d(-100, 20, zoom=1.1)
  
  writeWebGL(dir=file.path(tempdir(), "webGL"), width=500)
  return(Xpca)
}


plotLuvCols <- function(l,c,h,pca=0, col=hex(polarLUV(l,c,h)), ...){  
  
  hueRange <- diff(range(h))
  newHue <- h
  #shift by 90 degrees to see if the range gets smaller. If so, that implies that
  # perhaps the initial range was spanning the border from 360-0
  for (i in seq(0,330, by=30)){
    h2 <- h + i
    h2[h2 >= 360] <- h2[h2 >= 360] - 360
    h2Range <- diff(range(h2))
    if (h2Range < hueRange){
      hueRange <- h2Range
      newHue <- h2
    }
  }
  
  col <- col
  
  h <- newHue
  
  plot3d(l, c, h,          
         xlab="L", ylab="C", zlab="H", 
         type="l", lwd=2, col=col, xlim=c(0,100), ylim=c(0,160), zlim=c(0,360),...)
  
  plot3d(l, c, h,           
         type="p", add=TRUE, pch=19, size=6, col=col, ...)
  
  X <- cbind(l,c,h)
  #from Hans Borchers, http://r.789695.n4.nabble.com/Fit-a-3-Dimensional-Line-to-Data-Points-td863596.html
  N <- nrow(X) 
  
  meanX <- apply(X, 2, mean) 
  Xpca   <- princomp(X) 
  
  if (pca==TRUE){
    pca <- 1
  }
  
  if (pca){
    endpts <- list()
    for (pc in 1:pca){    
      dirVector <- Xpca$loadings[, pc] 
      t <- c(min(Xpca$score[, pc])-.2, max(Xpca$score[, pc])+.2) 
      endpts[[pc]] <- rbind(meanX + t[1]*dirVector, meanX + t[2]*dirVector)     
    }
    
    for (pc in 1:pca){  
      plot3d(endpts[[pc]][,1], endpts[[pc]][,2], endpts[[pc]][,3], type="l", add=TRUE, aspect=FALSE)
    }
    
    pv <- propVar(Xpca, pca)
    text3d(50,50,250, paste("R2 = ", round(pv,4), sep=""), aspect=FALSE)
  }
  
  view3d(-100, 20, zoom=1.1)
  
  writeWebGL(dir=file.path(tempdir(), "webGL"), width=500)
  return(Xpca)
}


showAllSequential <- function(){
  for (i in which(sequential[,2] == 9)){
    palette <- sequential[i:(i+8), 7:9]
    pca <-plotCols(palette$R,palette$G, palette$B, pca=1)
    cat(i, ": ", propVar(pca,1), "\n")
    readline()
  }
}

showAllDivergent <- function(){
  #all diverging series of 11
  for (i in which(diverging[,2] == 11)){
    palette <- diverging[i:(i+10), 7:9]
    pca <-plotCols(palette$R,palette$G, palette$B, pca=2)
    cat(i, ": ", propVar(pca,2), "\n")
    readline()
  }
}

load("../mat.Rda")

allSequential <- list()
for (i in which(sequential[,2] == 9)){
  palette <- sequential[i:(i+8), 7:9]
  allSequential[[length(allSequential)+1]] <- palette
}

#make symmetrical
diag(mat) <- 5.2
mat[,1] <- mat[1,]
mat[,2] <- mat[2,]
mat[,3] <- mat[3,]
mat[,4] <- mat[4,]
mat[,5] <- mat[5,]
mat <- mat - 1

mat <- mat*4.2
mat[mat<0] <- 0
mat <- mat+1

plotAllHeatmaps <- function(mat){
  for(i in 1:length(allSequential)){
    palette <- interpolateNewPalette(18, allSequential[[i]])
    png(filename=paste("palette-sym/palette-", i,".png", sep=""),
        width = 620, height = 620, units = "px",
        pointsize = 12, bg = "white")
    heatmap.r(mat,scale = "none", margins = c(4,4), col=rgb(palette/255))
    dev.off()
  }
}

#' Interpolate any number of colors from a provided color palette
#' @param n The number of colors desired in the new palette
#' @param palette a data.frame or matrix with each color occupying one row, and the columns being different color dimensions (RGB, HCL, etc.)
#' @return A data.frame with the same columns as the provided palette but with n interpolated rows representing n colors.
interpolateNewPalette <- function(n, palette){
  newPalette <- data.frame(matrix(NA, nrow=n, ncol=ncol(palette)))
  for (colIndex in 1:ncol(palette)){
    rspline <- smooth.spline(1:nrow(palette), palette[,colIndex], w=c(1,rep(.1, nrow(palette)-2), 1))
    newPalette[,colIndex] <- predict(rspline, seq(1, nrow(palette),length.out=n))$y
    
    #limit to [0,255]
    newPalette[newPalette[,colIndex]>255,colIndex] <- 255
    newPalette[newPalette[,colIndex]<0,colIndex] <- 0
  }
  colnames(newPalette) <- colnames(palette)
  newPalette
}

calcR2Sequential <- function(){
  R2 <- list()
  for (i in which(sequential[,2] == 9)){
    palette <- sequential[i:(i+8), 7:9]
    pca <-plotCols(palette$R,palette$G, palette$B, pca=1)
    cat(i, ": ", propVar(pca,1), "\n")
    R2[[length(R2)+1]] <- propVar(pca,1)
  }
  return(R2)
}


calcPathLength <- function(){
  plen <- array(dim=sum(sequential[,2] == 9, na.rm=TRUE))
  p <- 1
  for (i in which(sequential[,2] == 9)){
    palette <- sequential[i:(i+8), 7:9]    
    cat(i, ": ", getPathLength(palette), "\n")
    plen[p] <- getPathLength(palette)
    p <- p + 1
  }
  return(plen)
}


getPathLength <- function(palette){
  pd <- apply(palette, 2, diff)
  pl <- sqrt(apply(pd^2,1,sum))
  return(sum(pl))  
}
