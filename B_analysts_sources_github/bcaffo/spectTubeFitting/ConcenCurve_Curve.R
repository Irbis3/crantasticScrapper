#############################################################
# Aug 29 2011
# Jeff Goldsmith and Brian Caffo
#
# this file constructs the concentration by distance curve
# based on a curve fit through the center of a collection
# of points.
#############################################################

plot3d(sagitalSlice, coronalSlice, transverseSlice, 
  size = 3, axes = T, box = F, xlab = "", ylab = "", zlab = "", 
  xlim=range(0, 128), ylim=range(0, 128), zlim=range(0, 128)) 
lines3d(xValues, yValues, zValues, col = "red", lwd = 5)

spheres3d(xConstrained, yConstrained, zConstrained, col = "red", add = TRUE, radius = 2)

range = .1

#### find points located uniformly along the centerline
## create a vector of times separated uniformly between 0, 1
temp=0:2000/2000

## find the point on the curve for each time in 'temp'
pts=matrix(nrow=2001, ncol=3)
for(i in 1:2001){
	time=temp[i]
	
	## find the point on the fitted curve at 'time'
	pt=c(0,0,0)
	pt[1]=betaSagital[1,1] + betaSagital[2,1]*time + betaSagital[3,1]*(time)^2 + 		betaSagital[4,1]*(time)^3
	pt[2]=betaCoronal[1,1] + betaCoronal[2,1]*time + betaCoronal[3,1]*(time)^2 + 		betaCoronal[4,1]*(time)^3
	pt[3]=betaTransverse[1,1] + betaTransverse[2,1]*time + betaTransverse[3,1]*(time)^2 + 		betaTransverse[4,1]*(time)^3

	k=1
	while(time>attributes(basis)$knots[k] && k <= length(attributes(basis)$knots)){
		pt[1]=pt[1]+betaSagital[4+k]*(time-as.numeric(attributes(basis)$knots[k]))^3
		pt[2]=pt[2]+betaCoronal[4+k]*(time-as.numeric(attributes(basis)$knots[k]))^3
		pt[3]=pt[3]+betaTransverse[4+k]*(time-as.numeric(attributes(basis)$knots[k]))^3
		k=k+1
	}
	
	pts[i,]=pt
}

## find the distance along the curve for each time in 'temp' 
distBetweenPoints=vector("numeric", 2000)
distBetweenPoints[1]=dist2(pts[1,], pts[2,])
for(i in 2:2000){
	distBetweenPoints[i]=dist2(pts[i,], pts[i-1,])+distBetweenPoints[i-1]
	}
distBetweenPoints=c(0, distBetweenPoints)

## find points that are located uniformly along the curve
incrementBy=distBetweenPoints[2001]/round(distBetweenPoints[2001]*2)

indices=vector("numeric", round(distBetweenPoints[2001]*2))

for(i in 0:(round(distBetweenPoints[2001]*2)-1 )){
	temp=abs(distBetweenPoints-i*incrementBy)
	indices[i+1]=which(temp == min(temp))
}	

## set the vector of times we're interested in to those points found above
times=(indices-1)/2000
pts=pts[indices,]

distBetweenPoints=distBetweenPoints[indices]
rm(temp, indices)


concen=vector("numeric", length=length(times))

for(i in 1:length(times)){
	time=times[i]	
	timeInWindow=which(t > time - range & t < time + range)
	intenseInWindow=intensities[timeInWindow]
		
	## define weights for points in window
	weights=.5*(cos((t[timeInWindow]-time)*(pi/range))+1)
	
	## find concentration for each ellipse
	concen[i]=sum(intenseInWindow*weights)
}

concenNorm=(concen-min(concen))/(max(concen)-min(concen))
distBetweenPoints=distBetweenPoints * (3.45 / 10)

write.csv(cbind(distBetweenPoints, concenNorm, concen), 
  file=paste(image.file, "_ConcenCurve.csv", sep = ""))

plot(distBetweenPoints, concenNorm, type = 'l', xlab = "Distance", ylab = "Normalized Concentration")

#############################################################
#############################################################
#############################################################
#############################################################
#############################################################
#############################################################