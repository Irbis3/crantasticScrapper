#############################################################
# Aug 29 2011
# Jeff Goldsmith and Brian Caffo
#
# this file finds the distance between each image point and
# the centerline, and removes those points that are farther
# than a prespecified (anatomically motivated) distance.
#############################################################

## find points on curve corresponding to each data point
Dist = rep(NA, length(t))

## find points on curve corresponding to each data point
ctrPts=matrix(nrow=length(t), ncol=3)
for(i in 1:length(t)){
	time=t[i]
	
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
	
	ctrPts[i,]=pt
	
	Dist[i] = dist2(c(sagitalSlice[i], coronalSlice[i], transverseSlice[i]), pt)* (3.45 / 10)
}

FarPoints  = which(Dist > dist.threshold)
t = t[-FarPoints]
intensities = intensities[-FarPoints]
sagitalSlice = sagitalSlice[-FarPoints]
coronalSlice = coronalSlice[-FarPoints]
transverseSlice = transverseSlice[-FarPoints]

#############################################################
#############################################################
#############################################################
#############################################################
#############################################################
#############################################################