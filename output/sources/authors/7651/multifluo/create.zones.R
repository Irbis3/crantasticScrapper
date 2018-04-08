create.zones <-
function(img=NULL,shape="rect",n.zones=NULL,interact=TRUE,pts=NULL,name.zones=NULL)
{#img
	listRes=list()
	
	if(is.null(name.zones)||length(name.zones)!=n.zones){name.zones=rep(NULL,n.zones)}
	if(interact)
	{
		if(is.null(n.zones)){stop("Please enter a number of zones to be selected in the parameter n.zones")}
		for(i in 1:n.zones)
		{
			if(shape=="rect"|shape=="para"|shape=="circle")
			{
				listRes[[i]]=create.zone(img,shape=shape,name=name.zones[i],name.secours=i)
			}
			else
			{
				print("Please enter the type of the zone. 1 for rectangle, 2 for parallelogram, 3 for circle")
				shapeZoneNum=0
				while(!shapeZoneNum%in%c(1,2,3))
				{
					shapeZoneNum=scan(what="character",nmax=1)
					if(shapeZoneNum==1){shapeZone="rect"}
					if(shapeZoneNum==2){shapeZone="para"}
					if(shapeZoneNum==3){shapeZone="circle"}
				}				
				listRes[[i]]=create.zone(img,shape=shapeZone,name=name.zones[i],name.secours=i)
			}
		}	
	}
	else
	{
		pts=as.matrix(pts)
		n.zones=dim(pts)[1]
		if(shape=="rect")
		{
			for(i in 1:n.zones)
			{
				listRes[[i]]=create.zone(img,shape="rect",name=rownames(pts)[i],man=TRUE,x0=pts[i,1],y0=pts[i,2],x1=pts[i,3],y1=pts[i,4],x2=NULL,y2=NULL,x3=NULL,y3=NULL,name.secours=i)
			}	
		}
		if(shape=="circle")
		{
			for(i in 1:n.zones)
			{
				listRes[[i]]=create.zone(img,shape="circle",name=rownames(pts)[i],man=TRUE,x0=pts[i,1],y0=pts[i,2],x1=pts[i,3],y1=pts[i,4],x2=NULL,y2=NULL,x3=NULL,y3=NULL,name.secours=i)
			}
		
		
		}
		if(shape=="para")
		{
			for(i in 1:n.zones)
			{
				listRes[[i]]=create.zone(img,shape="para",name=rownames(pts)[i],man=TRUE,x0=pts[i,1],y0=pts[i,2],x1=pts[i,3],y1=pts[i,4],x2=pts[i,5],y2=pts[i,6],x3=NULL,y3=NULL,name.secours=i)
			}
		
		}
	}	
	#names(listRes)=nameZone
	return(listRes)
}
