img.zone <-
function(zone,d=NULL,mask=NULL,edge.only=NULL,wo.edge=NULL,graph=FALSE,img=NULL,lim=30)
{ # return the positions of parallelogram/rectangles
	# thanks to the point positions enterend in the zone matrix
	if(!is.null(img)){d=c(dim(img)[1],dim(img)[2])}
	matricePos=matrix(NA,d[1],d[2])
	nbRect=length(zone)
	edge=function(img,limit=30,graph=FALSE)
	{# edge(img=count.img,threshold=30)
		gray.count=grayscale(img)
		dx=imgradient(gray.count,"x")
		dy=imgradient(gray.count,"y")
		grad=sqrt(dx^2+dy^2)
		#plot(grad)
		dim(as.array(grad))
		new.grad=as.matrix(grad[,,1,1])
		new.grad.bool=new.grad>limit
		if(graph)
		{
			plotimage(new.grad.bool)
		}	
		return(new.grad.bool)
	}
	if(!is.null(edge.only)|!is.null(wo.edge))
	{
		edges=edge(img=img,limit=lim)
	}else{edges=matrix(TRUE,d[1],d[2])}
	for(i in 1:nbRect) # a chaque zone
	{
		shape=zone[[i]]$shape
		edge.only.i=zone[[i]]$name%in%edge.only
		without.edge.i=zone[[i]]$name%in%wo.edge
		if(shape=="para")
		{
			pointA=c(zone[[i]]$pts[,2])
			pointB=c(zone[[i]]$pts[,1])
			pointC=c(zone[[i]]$pts[,3])
			vectAB=pointB-pointA
			vectAC=pointC-pointA
			pointD=pointA+vectAB+vectAC
			
			imin=min(pointA[1],pointB[1],pointC[1],pointD[1])
			jmin=min(pointA[2],pointB[2],pointC[2],pointD[2])
			imax=max(pointA[1],pointB[1],pointC[1],pointD[1])
			jmax=max(pointA[2],pointB[2],pointC[2],pointD[2])
			for(ind in imin:imax)
			{
				for(jind in jmin:jmax)
				{
					intPara=interieur.para(ind,jind,pointA[1],pointA[2],pointB[1],pointB[2],pointC[1],pointC[2])
					edge.cond=TRUE
					if(edge.only.i){edge.cond=edges[ind,jind]}
					if(without.edge.i){edge.cond=!edges[ind,jind]}
					if(intPara&edge.cond)
					{
						matricePos[ind,jind]=zone[[i]]$name
					}
				}
			}
		}
		
		
		if(shape=="rect")
		{
			pointA=c(zone[[i]]$pts[,1])
			pointB=c(zone[[i]]$pts[,2])
			imin=min(pointA[1],pointB[1])
			jmin=min(pointA[2],pointB[2])
			imax=max(pointA[1],pointB[1])
			jmax=max(pointA[2],pointB[2])
			for(ind in imin:imax)
			{
					for(jind in jmin:jmax)
					{
							edge.cond=TRUE
							if(edge.only.i){edge.cond=edges[ind,jind]}
							if(without.edge.i){edge.cond=!edges[ind,jind]}
							if(edge.cond)
							{
								matricePos[ind,jind]=zone[[i]]$name
							}	
					}
			}	
		}
		if(shape=="circle")
		{
			centreCercle=c(zone[[i]]$pts[,1])
			pointCercle=c(zone[[i]]$pts[,2])
			rayon=sqrt(sum((pointCercle-centreCercle)^2))
			imin=round(centreCercle[1]-rayon)
			jmin=round(centreCercle[2]-rayon)
			imax=round(centreCercle[1]+rayon)
			jmax=round(centreCercle[2]+rayon)
			for(ind in imin:imax)
			{
					for(jind in jmin:jmax)
					{
						bool=(ind-centreCercle[1])^2+(jind-centreCercle[2])^2<rayon^2
						edge.cond=TRUE
						if(edge.only.i){edge.cond=edges[ind,jind]}
						if(without.edge.i){edge.cond=!edges[ind,jind]}
						if(bool&edge.cond){matricePos[ind,jind]=zone[[i]]$name}	
					}
			}	
		}
	
		
		if(zone[[i]]$name%in% wo.edge&&zone[[i]]$name%in% edge.only)
		{
				stop("A zone can not be in without.edge and in edge.only")
		}
	}

	if(!is.null(mask))
	{
		matricePos[!mask]=NA
	}
	return(matricePos)
}
