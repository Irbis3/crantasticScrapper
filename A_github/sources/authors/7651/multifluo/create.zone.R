create.zone <-
function(image.to.plot,shape="rect",name=NULL,man=FALSE,x0=NULL,y0=NULL,x1=NULL,y1=NULL,x2=NULL,y2=NULL,x3=NULL,y3=NULL,name.secours=NULL)
{ #create.zone(image.to.plot,shape="rectangle")
	zone=list()	
	if(!shape %in% c("rect","para","quad","circle")){stop("Wrong shape name. Please choose a shape between 'rect','para','circle','quad'")}

	
	if(shape=="rect")
	{
		matRes=matrix(NA,2,2);rownames(matRes)=c("x","y");colnames(matRes)=c("pt1","pt2")
		if(!man)
		{
			rectangle=grabRect(image.to.plot)
			matRes[1,1]=rectangle["x0"]
			matRes[2,1]=rectangle["y0"]
			matRes[1,2]=rectangle["x1"]
			matRes[2,2]=rectangle["y1"]
		}
		else
		{
			matRes[1,1]=x0
			matRes[2,1]=y0
			matRes[1,2]=x1
			matRes[2,2]=y1
		}
		
	}
	if(shape=="para")
	{
	
		matRes=matrix(NA,2,3);rownames(matRes)=c("x","y")
		if(!man)
		{
			for(j in 1:3)
			{
					point=grabPoint(image.to.plot)
					matRes[1,j]=point[1]
					matRes[2,j]=point[2]			
			}		
		}
		if(man)
		{
			matRes[1,1]=x0
			matRes[2,1]=y0
			matRes[1,2]=x1
			matRes[2,2]=y1	
			matRes[1,3]=x2
			matRes[2,3]=y2
		
		}
	}
	# if(shape=="quad")
	# {
		# matRes=matrix(NA,2,4);rownames(matRes)=c("x","y")
		# if(!man)
		# {
			# for(j in 1:4)
			# {
				
					# point=grabPoint(image.to.plot)
					# matRes[1,j]=point[1]
					# matRes[2,j]=point[2]
			# }
		# }
	# }
	if(shape=="circle")
	{
		if(!man)
		{
			matRes=matrix(NA,2,2);colnames(matRes)=c("center","pt")
			lineGrabbed=grabLine(image.to.plot)
			matRes[1,1]=lineGrabbed["x0"]
			matRes[2,1]=lineGrabbed["y0"]
			matRes[1,2]=lineGrabbed["x1"]
			matRes[2,2]=lineGrabbed["y1"]	
		}
		if(man)
		{
			matRes[1,1]=x0
			matRes[2,1]=y0
			matRes[1,2]=x1
			matRes[2,2]=y1
			matRes[1,3]=x2
			matRes[2,3]=y2	
		}	
	}

	if(is.null(name))
	{
		print("Enter the name of this zone")
		nameZone=scan(what="character",nmax=1)
		if(length(nameZone)==0){nameZone=name.secours}
	}else{nameZone=name}
	
	zone[["pts"]]=matRes
	zone[["name"]]=nameZone
	zone[["shape"]]=shape
	zone[["img"]]=image.to.plot
	return(zone)
}
