plotzone <-
function(zone,name=FALSE, dim.img=NULL,d=200,cex=1,col="black",main="Zone",lwd=1)
{
	#trace un parallelogramme
	# zone a 6 colonnes (une pour chaque point)
	main="Zone"
	if(dev.cur()==1){plot(NULL,xlim=c(0,dim.img[1]),ylim=c(0,dim.img[2]),main=main)}
	nbRect=length(zone)	
	for(i in 1:nbRect)
	{
		type=zone[[i]]$shape
		if(type!="circle")
		{
			if(type=="para"||type=="quad"){pointC=zone[[i]]$pts[,3]}
			if(type=="quad"){pointD=zone[[i]]$pts[,4]}
			# cas ou on a des rectangle
			if(type=="rect")
			{
				pointA=zone[[i]]$pts[,1]
				pointD=zone[[i]]$pts[,2]
				pointB=pointC=pointA
				pointB[1]=pointD[1]
				pointC[2]=pointD[2]
			}
		
			if(type=="para")
			{
				pointA=zone[[i]]$pts[,2]
				pointB=zone[[i]]$pts[,1]
				pointC=zone[[i]]$pts[,3]
				vectAB=pointB-pointA
				vectAC=pointC-pointA
				pointD=pointA+vectAB+vectAC
			}
			if(type=="quad")
			{
				pointC=zone[[i]]$pts[,3]
				pointD=zone[[i]]$pts[,4]
			}
		
			segments(pointA[1],pointA[2],pointB[1],pointB[2],col=col,lwd=lwd)
			segments(pointA[1],pointA[2],pointC[1],pointC[2],col=col,lwd=lwd)
			segments(pointC[1],pointC[2],pointD[1],pointD[2],col=col,lwd=lwd)
			segments(pointB[1],pointB[2],pointD[1],pointD[2],col=col,lwd=lwd)
		
		}
		if(type=="circle")
		{
			centre=zone[[i]]$pts[,1]
			pointA=zone[[i]]$pts[,2]
			pointC=NULL
			pointD=NULL
			pointB=NULL
			rayon=sqrt((centre[1]-pointA[1])^2+(centre[2]-pointA[2])^2)
			xPoints=centre[1]+rayon*cos(2*pi*1:100/100)
			yPoints=centre[2]+rayon*sin(2*pi*(1:100)/100)
			points(xPoints,yPoints, type="l" ,col=col)
		
		}
		if(name)
		{
				offSetY=dim.img[2]/d
				text(mean(c(pointA[1],pointB[1],pointC[1],pointD[1])),min(pointA[2],pointB[2],pointC[2],pointD[2])-offSetY,zone[[i]]$name,cex=cex,col=col)
		}
		
	}
}
