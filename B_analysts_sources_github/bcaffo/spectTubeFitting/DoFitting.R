#############################################################
# Aug 29 2011
# Jeff Goldsmith and Brian Caffo
#
# this file is gives an interactive way of specifying 
# endpoints for the fitting of a centerline. next, it
# sources the collection of files to fit this line, remove
# outlying points, refit the centerline, and construct a
# concentration-by-distance curve.
#############################################################

helper.dir = sub('DoFitting.R', '', helper.file, fixed = TRUE)

source(paste(helper.dir, "tps.R", sep = ""))
source(paste(helper.dir, "Dist.R", sep = ""))

## read in the image
img <- f.read.analyze.volume(image.file)[,,,1]


#############################################################
# threshold the image and construct slices
#############################################################

nopoints <- 1000

##set the data that is less than 0 to 0
img[img < 0] <- 0

##size must be the same for all three dimensions
size <- dim(img)[1]

##theshold again to minimize computing
##threshold <- 0
threshold <- 8
timg <- img
#timg[img <= threshold] <- 0

##only do this if you want to just work with a sample of points
##pics among the points above a threshold
locs <- timg > 0
locs <- (1 : size ^ 3)[locs]

##do this if you want to weight locations by their intesinities
#probs <- timg[locs] / sum(timg[locs])
sampledLocs <- sample(locs, min(length(locs), nopoints))
timg <- array(0, rep(size, 3))
timg[sampledLocs] <- 1

##those pixels where data occurs
where <- timg > 0

##keep track of intensities of points used
intensities=img[where]

##sneaky way to get the indices of the data points surviving the threshold
##without looping
temp <- expand.grid(1 : 128, 1 : 128, 1 : 128)
sagitalSlice <- array(temp[,1], rep(size, 3))
#image(sagitalSlice[1,,])##check, should be constant
sagitalSlice <- sagitalSlice[where]
coronalSlice <- array(temp[,2], rep(size, 3))
#image(coronalSlice[,1,])##check, should be constant
coronalSlice <- coronalSlice[where]
transverseSlice <- array(temp[,3], rep(size, 3))
#image(transverseSlice[,,1])##check, should be constant
transverseSlice <- transverseSlice[where]
##free up this memory
rm(temp)

##the order of the transverse slices, useful for starting out fitting up the colon
odr <- order(transverseSlice)

##starting time, this is in that order
t <- seq(0, 1, length = length(sagitalSlice))

##reorder it so that it's back in the correct order
##now t should be such that lower values are in lower
##transeverse slices
t <- t[order(odr)]


#############################################################
# GUI for removing point sources
#############################################################

x = 1

plot3d(sagitalSlice, coronalSlice, transverseSlice, 
  size = 3, axes = T, box = F, xlab = "", ylab = "", zlab = "", 
  xlim=range(0, 128), ylim=range(0, 128), zlim=range(0, 128)) 
persp3d(x = c(x,x+1), y = c(0, 129), z = c(0, 129, 0, 129), add = TRUE, alpha = .3)  
     
removeSource <- TRUE
Slice <- NULL
while (removeSource){
  ##now get the point on the axial
  image(1 : 128, 1 : 128, timg[x+1,,], col = grey(c(0, seq(.1, 1, by = .1))),
        axes = FALSE, frame = TRUE, xlab = "Click here to remove forward points and select end points", ylab = "")
  abline(h = c(10, 118), col = "red")
  text(64, 120, "Go left a slice", col = "white")
  text(120, 120, "x10", col = "white")
  text(64, 5, "Go right a slice", col = "white")
  text(120, 5, "x10", col = "white")
  text(64, 110, "Down", col = "white")
  text(64, 15, "Up", col = "white")
  text(5, 64, "Front", col = "white")
  text(120, 64, "Back", col = "white")

  out <- locator(n = 1, type = "n")
  ##below the blue line increases z (but goes down the body toward feet)
  if (out$y < 10 & out$y > 0 & out$x < 115) {
    x <- min(x + 1, 128)
    plot3d(sagitalSlice, coronalSlice, transverseSlice, 
      size = 3, axes = T, box = F, xlab = "", ylab = "", zlab = "", 
      xlim=range(0, 128), ylim=range(0, 128), zlim=range(0, 128)) 
    persp3d(x = c(x,x+1), y = c(0, 129), z = c(0, 129, 0, 129), add = TRUE, alpha = .3)  
  }
  else if (out$y > 0 & out$y < 10 & out$x > 115 & out$x < 129) {
    x <- max(x + 10, 1)
    plot3d(sagitalSlice, coronalSlice, transverseSlice, 
      size = 3, axes = T, box = F, xlab = "", ylab = "", zlab = "", 
      xlim=range(0, 128), ylim=range(0, 128), zlim=range(0, 128)) 
    persp3d(x = c(x,x+1), y = c(0, 129), z = c(0, 129, 0, 129), add = TRUE, alpha = .3)  
  }

  ##above the blue line decreases z (towards the head)
  else if (out$y > 118 & out$y < 129 & out$x < 115) {
    x <- max(x - 1, 1)
    plot3d(sagitalSlice, coronalSlice, transverseSlice, 
      size = 3, axes = T, box = F, xlab = "", ylab = "", zlab = "", 
      xlim=range(0, 128), ylim=range(0, 128), zlim=range(0, 128)) 
    persp3d(x = c(x,x+1), y = c(0, 129), z = c(0, 129, 0, 129), add = TRUE, alpha = .3)  
  }
  else if (out$y > 118 & out$y < 129 & out$x > 115 & out$x < 129) {
    x <- max(x - 10, 1)
    plot3d(sagitalSlice, coronalSlice, transverseSlice, 
      size = 3, axes = T, box = F, xlab = "", ylab = "", zlab = "", 
      xlim=range(0, 128), ylim=range(0, 128), zlim=range(0, 128)) 
    persp3d(x = c(x,x+1), y = c(0, 129), z = c(0, 129, 0, 129), add = TRUE, alpha = .3)  
  }


  ## below means we're done
  else if (out$y < 0){
    remove = which(sagitalSlice < x)
    
    if(length(remove)>0){
      sagitalSlice = sagitalSlice[-remove]
      coronalSlice = coronalSlice[-remove]
      transverseSlice = transverseSlice[-remove]
      intensities = intensities[-remove]
      t = t[-remove]
    }

    removeSource <- FALSE
  }
}



#############################################################
# GUI for selecting endpoints
#############################################################

z = 64

plot3d(sagitalSlice, coronalSlice, transverseSlice, 
  size = 3, axes = T, box = F, xlab = "", ylab = "", zlab = "", 
  xlim=range(0, 128), ylim=range(0, 128), zlim=range(0, 128)) 
persp3d(c(0, 129), y = c(0, 129), z = matrix(rep(z, 4), 2, 2), add = TRUE, alpha = .3)  
 
getPoints <- TRUE
pts <- NULL
while (getPoints){
  ##now get the point on the axial
  image(1 : 128, 1 : 128, timg[,,z], col = grey(c(0, seq(.1, 1, by = .1))),
        axes = FALSE, frame = TRUE, xlab = "Click here to set endpoints and continue", ylab = "")
  abline(h = c(10, 118), col = "red")
  text(64, 120, "Go up a slice", col = "white")
  text(120, 120, "x10", col = "white")
  text(64, 5, "Go down a slice", col = "white")
  text(120, 5, "x10", col = "white")
  text(64, 110, "Front", col = "white")
  text(64, 15, "Back", col = "white")
  text(5, 64, "Right", col = "white")
  text(120, 64, "Left", col = "white")
  mtext(side = 4, "Undo Last Point")
  out <- locator(n = 1, type = "n")
  ##below the blue line increases z (but goes down the body toward feet)
  if (out$y < 10 & out$y > 0 & out$x < 115) {
    z <- min(z + 1, 128)
    plot3d(sagitalSlice, coronalSlice, transverseSlice, 
      size = 3, axes = T, box = F, xlab = "", ylab = "", zlab = "", 
      xlim=range(0, 128), ylim=range(0, 128), zlim=range(0, 128)) 
    persp3d(c(0, 129), c(0, 129), z = matrix(rep(z, 4), 2, 2), add = TRUE, alpha = .3)
    if(!is.null(pts)){
      spheres3d(pts[,1], pts[,2], pts[,3], col = "red", add = TRUE, radius = 2) 
    }
  }
  else if (out$y > 0 & out$y < 10 & out$x > 115 & out$x < 129) {
    z <- max(z + 10, 1)
    plot3d(sagitalSlice, coronalSlice, transverseSlice, 
      size = 3, axes = T, box = F, xlab = "", ylab = "", zlab = "", 
      xlim=range(0, 128), ylim=range(0, 128), zlim=range(0, 128)) 
    persp3d(c(0, 129), c(0, 129), z = matrix(rep(z, 4), 2, 2), add = TRUE, alpha = .3)
    if(!is.null(pts)){
      spheres3d(pts[,1], pts[,2], pts[,3], col = "red", add = TRUE, radius = 2) 
    }
  }

  ##above the blue line decreases z (towards the head)
  else if (out$y > 118 & out$y < 129 & out$x < 115) {
    z <- max(z - 1, 1)
    plot3d(sagitalSlice, coronalSlice, transverseSlice, 
      size = 3, axes = T, box = F, xlab = "", ylab = "", zlab = "", 
      xlim=range(0, 128), ylim=range(0, 128), zlim=range(0, 128)) 
    persp3d(c(0, 129), c(0, 129), z = matrix(rep(z, 4), 2, 2), add = TRUE, alpha = .3)
    if(!is.null(pts)){
      spheres3d(pts[,1], pts[,2], pts[,3], col = "red", add = TRUE, radius = 2) 
    }
  }
  else if (out$y > 118 & out$y < 129 & out$x > 115 & out$x < 129) {
    z <- max(z - 10, 1)
    plot3d(sagitalSlice, coronalSlice, transverseSlice, 
      size = 3, axes = T, box = F, xlab = "", ylab = "", zlab = "", 
      xlim=range(0, 128), ylim=range(0, 128), zlim=range(0, 128)) 
    persp3d(c(0, 129), c(0, 129), z = matrix(rep(z, 4), 2, 2), add = TRUE, alpha = .3)
    if(!is.null(pts)){
      spheres3d(pts[,1], pts[,2], pts[,3], col = "red", add = TRUE, radius = 2) 
    }
  }


  ## below means we're done
  else if (out$y < 0){
    getPoints <- FALSE
  }
  ##clicking to the right removes the previous point from the list
  else if (out$x > 128){
    if(nrow(pts) == 1){
      pts <- NULL
      plot3d(sagitalSlice, coronalSlice, transverseSlice, 
        size = 3, axes = T, box = F, xlab = "", ylab = "", zlab = "", 
        xlim=range(0, 128), ylim=range(0, 128), zlim=range(0, 128)) 
      persp3d(c(0, 129), c(0, 129), z = matrix(rep(z, 4), 2, 2), add = TRUE, alpha = .3)  
    } else {
      pts <- pts[-nrow(pts),]
      if(is.null(nrow(pts))){
        pts = matrix(pts, nrow =1, ncol =3)
      }
      plot3d(sagitalSlice, coronalSlice, transverseSlice, 
        size = 3, axes = T, box = F, xlab = "", ylab = "", zlab = "", 
        xlim=range(0, 128), ylim=range(0, 128), zlim=range(0, 128)) 
      persp3d(c(0, 129), c(0, 129), z = matrix(rep(z, 4), 2, 2), add = TRUE, alpha = .3) 
      spheres3d(pts[,1], pts[,2], pts[,3], col = "red", add = TRUE, radius = 2)
    }
  }
  ##take the current point and append
  else {
    pts <- rbind(pts, c(unlist(out), z))
    spheres3d(out$x, out$y, z, col = "red", add = TRUE, radius = 2)
  }
}

colnames(pts) <- c("x", "y", "z")

n.pts = dim(pts)[1]

##set the endpoints
xEndpoints <- pts[c(1, n.pts),1]
yEndpoints <- pts[c(1, n.pts),2]
zEndpoints <- pts[c(1, n.pts),3]
tEndpoints <- 0 : 1

## set constrained interior points
if(n.pts>2){
  xConstrainedNoT <- pts[2:(n.pts-1),1]
  yConstrainedNoT <- pts[2:(n.pts-1),2]
  zConstrainedNoT <- pts[2:(n.pts-1),3]
  
  tConstrainedNoT <- seq(0, 1, length=n.pts)[2:(n.pts-1)]
} else {
  xConstrainedNoT <- NULL
  yConstrainedNoT <- NULL
  zConstrainedNoT <- NULL
  tConstrainedNoT <- NULL
}

xConstrained <- c(xEndpoints, xConstrainedNoT)
yConstrained <- c(yEndpoints, yConstrainedNoT)
zConstrained <- c(zEndpoints, zConstrainedNoT)

tConstrained <- c(0, 1, tConstrainedNoT)

colnames(pts) = c("X", "Y", "Z")
rownames(pts) = paste("Point", 1:n.pts)

write.csv(pts, file=paste(image.file, "_EndPoints.csv", sep = ""))

dev.off()

#############################################################
# curve-fitting and concentration by distance
#############################################################

if(PARABOLA){
  source(paste(helper.dir,"CurveFit_Parabola.R", sep=""))
  source(paste(helper.dir,"RemovePts_Parabola.R", sep=""))
  source(paste(helper.dir,"CurveFit_Parabola.R", sep=""))
  source(paste(helper.dir,"ConcenCurve_Parabola.R", sep=""))
} else {
  source(paste(helper.dir,"CurveFit_Curve.R", sep=""))
  source(paste(helper.dir,"RemovePts_Curve.R", sep=""))
  source(paste(helper.dir,"CurveFit_Curve.R", sep=""))
  source(paste(helper.dir,"ConcenCurve_Curve.R", sep=""))
}


#############################################################
#############################################################
#############################################################
#############################################################
#############################################################
#############################################################