col <- read.csv("../ColorPalettes.csv", stringsAsFactors=FALSE)
sequential <- col[1:756,]
diverging <- col[757:1323,]
qualitative <- col[1324:1689,]


load("../mat.sym.Rda")

allSequential <- list()
for (i in which(sequential[,2] == 9)){
  palette <- sequential[i:(i+8), 7:9]
  allSequential[[length(allSequential)+1]] <- palette
}



plotLuvCols <- function(l,c,h,pca=0, col=hex(polarLUV(l,c,h)), ...){  
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

plotCols <- function(red,green,blue,pca=0, col=rgb(red/255, green/255, blue/255), ...){  
  library(rgl)
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
  
  if (pca > 0){
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

#' Convert RGB color pallete to polarLUV
getSequentialLuv <- function(index, paletteList){
  pal <- as.matrix(paletteList[[index]])/255
  pal <- RGB(pal)
  
  #black doesn't convert to polarLUV correctly, manually check
  black <- which(hex(pal) == "#000000")
  
  pal <- as(pal, "polarLUV")
  pal@coords[black,] <- c(0, 0, mean(pal@coords[-black,"H"]))
  pal
}


RGBToLuv <- function(r, g, b){  
  if (class(r) == "data.frame" && missing(g) && missing(b)){
    r <- as.matrix(r)        
    if(any(r > 1)){
      r <- r/255
    }
    col <- RGB(r)
  } else{
    if(any(r > 1)){
      r <- r/255
      g <- g/255
      b <- b/255
    }
    col <- RGB(r, g, b)
  }
  
  #black doesn't convert to polarLUV correctly, manually check
  black <- which(hex(col) == "#000000")
  
  col <- as(col, "polarLUV")
  col@coords[black,] <- c(0, 0, mean(col@coords[-black,"H"]))
  col@coords
}
