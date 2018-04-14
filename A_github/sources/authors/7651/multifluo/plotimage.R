plotimage <-
function(mat,lim=NULL,nc=1000,m=NULL,z=FALSE,add=FALSE,cols="rainbow",lc=c(0,0.7),p="l")
{ # mat : matrice a tracer
	
	if(!p%in%c("l","a","al","n")){p="n"}
	if(p=="l"){	axes=FALSE;leg=TRUE}
	if(p=="a"){	axes=TRUE;leg=TRUE}
	if(p=="al"){	axes=TRUE;leg=TRUE}
	if(p=="n"){	axes=FALSE;leg=FALSE}
	alpha.col=1
	continuous=TRUE
	main=NULL
	imageToPlot=NULL
	nbPixX=dim(mat)[1]
	nbPixY=dim(mat)[2]
	if(is.null(lim)){lim=c(NULL,NULL)}
	min.scale=lim[1]
	max.scale=lim[2]
	start.col=lc[1]
	end.col=lc[2]
	if(is.null(m)){m=matrix(TRUE,dim(mat)[1],dim(mat)[2])}
	if(!z)
	{
		if(is.null(min.scale)){min.scale=min(mat,na.rm=T)}
		if(is.null(max.scale)){max.scale=max(mat,na.rm=T)}
		if(cols=="rainbow")
		{
			vecteurCouleur=rev(rainbow(nc,start=start.col,end=end.col,alpha=alpha.col))
		}
		if(cols=="heat.colors")
		{
			vecteurCouleur=heat.colors(nc, alpha = alpha.col)
		}
		if(cols=="terrain.colors")
		{
			vecteurCouleur=terrain.colors(nc, alpha = alpha.col)
		}
		if(cols=="topo.colors")
		{
			vecteurCouleur=topo.colors(nc, alpha = alpha.col)
		}
		if(cols=="cm.colors")
		{
			vecteurCouleur=cm.colors(nc, alpha = alpha.col)
		}
		if(cols=="gray.scale")
		{
			vecteurCouleur=gray.colors(nc, start = start.col, end = end.col, gamma = 2.2, alpha = alpha.col)
		}
		#if(col.rev){vecteurCouleur=rev(vecteurCouleur)}
	
		pas=(max.scale-min.scale)/nc
	
		if(!add)
		{
			if(leg)
			{
				matrice <- matrix(c(1,2,2,2,2,2),nrow=1,ncol=6,byrow=TRUE)
				layout(matrice)
				par(mar=c(0,0,0,0))
				par(xaxt="n")
				par(yaxt="n")
				par(bty="n")
			
			plot(rep(1,nc),1:nc,col=vecteurCouleur)
			if(abs(nc*pas)<10){digits=1}else{digits=0}
			for(k in 0:(nc/100))
			{		
				text(0.7,k*100,round(min.scale+100*k*pas,digits=digits))
			}

			par(mar=c(3,2,0,2))
			
			par(xaxt="s")
			par(yaxt="s")
			}
		
			
			
			if(!continuous & !add)
			{
				plot(NULL,xlim=c(0,nbPixX),ylim=c(0,nbPixY),xlab="",ylab="",main=main)
			}
			
		}
		matR=matB=matG=matrix(0,nbPixX,nbPixY)
		
		for(i in 1:nbPixX)
		{
			for(j in 1:nbPixY)
			{
				tim=mat[i,j]
				
				if(!is.na(tim)&(m[i,j]))
				{
					if(tim<=min.scale){indicecol=1}
					if(tim>=max.scale){indicecol=nc}
					if(tim<max.scale&tim>min.scale){indicecol=round((tim-min.scale)/pas)}
					if(indicecol>nc){indicecol=nc}
					if(indicecol<1){indicecol=1}
					if(continuous&!add)
					{
						matR[i,j]=col2rgb(vecteurCouleur[indicecol])["red",]
						matG[i,j]=col2rgb(vecteurCouleur[indicecol])["green",]
						matB[i,j]=col2rgb(vecteurCouleur[indicecol])["blue",]
					}
					else
					{
						points(i,j,col=vecteurCouleur[indicecol],pch=20,cex=0.5)
					}
					
					
				}
				
			}
		}
	
		imageToPlot=as.cimg(c(as.vector(unlist(matR)),as.vector(unlist(matG)),as.vector(unlist(matB))),x=nbPixX,y=nbPixY,cc=3)
		if(!add&continuous)
		{
			par(xaxt="s")
			par(yaxt="s")
			par(mar=c(2,2,3,2))
			plot(imageToPlot,axes=axes,new=add,main=main)
		}	
	
	}
	if(z)
	{
		zones=levels(as.factor(as.vector(mat)))
		couleurs=rainbow(length(zones))
		matR=matB=matG=matrix(0,nbPixX,nbPixY)
		matrice <- matrix(c(1,1,1,1,1,1),nrow=1,ncol=1,byrow=TRUE)
		layout(matrice)
		plot(NULL,xlim=c(0,nbPixX),ylim=c(0,nbPixY),xlab="",ylab="")
		for(i in 1:nbPixX)
		{
			for(j in 1:nbPixY)
			{
				tim=mat[i,j]	
				if(!is.na(tim)&m[i,j])
				{
				
					for(k in 1:length(zones))
					{
					
						if(tim==zones[k])
						{
							
							if(continuous&!add)
							{
								matR[i,j]=as.data.frame(col2rgb(couleurs[k]))["red",]
								matG[i,j]=as.data.frame(col2rgb(couleurs[k]))["green",]
								matB[i,j]=as.data.frame(col2rgb(couleurs[k]))["blue",]
								imageToPlot=as.cimg(c(as.vector(unlist(matR)),as.vector(unlist(matG)),as.vector(unlist(matB))),x=nbPixX,y=nbPixY,cc=3)
							}	
							else
							{
								if(!add){plot(NULL,xlim=c(0,nbPixX),ylim=c(0,nbPixY),xlab="",ylab="",main=main)}
								points(mat[i],mat[j],col=couleurs[k],cex=0.6)
								matR[i,j]=as.data.frame(col2rgb(couleurs[k]))["red",]
								matG[i,j]=as.data.frame(col2rgb(couleurs[k]))["green",]
								matB[i,j]=as.data.frame(col2rgb(couleurs[k]))["blue",]
								imageToPlot=as.cimg(c(as.vector(unlist(matR)),as.vector(unlist(matG)),as.vector(unlist(matB))),x=nbPixX,y=nbPixY,cc=3)	
							}
						}
					}	
				}
			}
		}
		
		if(continuous&&!add)
		{
			par(mar=c(1,1,3,1))
			plot(imageToPlot,axes=axes)
			#axis(1)
			#axis(2)
		}		
	}
	return(imageToPlot)
}
