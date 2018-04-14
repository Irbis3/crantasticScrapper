intpca <-
function(vars,ind,map="b",si=NULL,ic=NULL,sic=NULL,ell=NULL,seg=NULL,epd=NULL,x="",y="")
{
	par(mar=c(4,4,1,1))
	par(mgp=c(1, 1, 0))
	par(oma=c(2,1,3,0))
	indLabels=TRUE
	varsLabels=TRUE
	if(!map%in%c("bil","bip","b","til","t","tip")){map="b";warning("Wrong map name. Please choose between 'b','bil','bip','t','til',tip'.")}
	if(map=="bil"){		si=TRUE;siLabels=TRUE;biplot=TRUE}
	if(map=="bip"){		si=TRUE;siLabels=FALSE;biplot=TRUE}
	if(map=="b"){		si=FALSE;siLabels=FALSE;biplot=TRUE}
	if(map=="til"){		si=TRUE;siLabels=TRUE;biplot=FALSE}
	if(map=="tip"){		si=TRUE;siLabels=FALSE;biplot=FALSE}
	if(map=="t"){		si=FALSE;siLabels=FALSE;biplot=FALSE}	
	
	#if(returnX){ind[,1]=-ind[,1];vars[,1]=-vars[,1];if(!is.null(ell)){ell[,,1]=-ell[,,1]}}
	#if(returnY){ind[,2]=-ind[,2];vars[,2]=-vars[,2];if(!is.null(ell)){ell[,,2]=-ell[,,2]}} mainTitle
	pchVect=3
	if(is.null(epd))
	{
		if(biplot)
		{
		  max.norm.prod=max(abs(ind[,1]))
		  max.norm.suj=max(abs(vars[,1]))
		  epd.conseil=max.norm.prod/max.norm.suj
		  epd=round(epd.conseil,digits=2)  
		}
		if(!biplot)
		{
			epd=1
		}
	}
		
	# Labels
	labelx=rownames(ind)
	labely=rownames(vars)
	nb.var=dim(vars)[1]
	nb.ind=dim(ind)[1]
	
	# Couleurs
	varsColors=rep("red",nb.var)
	if(is.null(ic)){ic=rep("blue",nb.ind)}
	if(is.null(sic)){sic=rainbow(nb.ind)}
	
	
	# Coordonnees
	xMinInd=min(ind[,1])
	xMaxInd=max(ind[,1])
	yMinInd=min(ind[,2])
	yMaxInd=max(ind[,2])
	xMinVar=min(vars[,1]*epd)
	xMaxVar=max(vars[,1]*epd)
	yMinVar=min(vars[,2]*epd)
	yMaxVar=max(vars[,2]*epd)
	
	if (!is.null(ell))
	{
		xMinInd=min(xMinInd,min(ell[,,1]))
		xMaxInd=max(xMaxInd,max(ell[,,1]))
		yMinInd=min(yMinInd,min(ell[,,2]))
		yMaxInd=max(yMaxInd,max(ell[,,2]))
	}
		
	if (si)
	{
		xMinInd=min(si[[1]][,1])
		xMaxInd=max(si[[1]][,1])
		yMinInd=min(si[[1]][,2])
		yMaxInd=max(si[[1]][,2])
		#if(returnX){si[[1]][,1]=-si[[1]][,1]}
		#if(returnY){si[[1]][,2]=-si[[1]][,2]}
		
		for(i in 2:nb.ind)
		{
			xMinInd=min(xMinInd,min(si[[i]][,1]))
			xMaxInd=max(xMaxInd,max(si[[i]][,1]))
			yMinInd=min(yMinInd,min(si[[i]][,2]) )
			yMaxInd=max(yMaxInd,max(si[[i]][,2]))
			#if(returnX){si[[i]][,1]=-si[[i]][,1]}
			#if(returnY){si[[i]][,2]=-si[[i]][,2]}
		}
	}
	
	
	if (biplot==TRUE)
	{
		# Tous les points sur une carte
		xMinC1=min(xMinInd,xMinVar)*1.1
		xMaxC1=max(xMaxInd,xMaxVar)*1.1
		yMinC1=min(yMinInd,yMinVar)*1.1
		yMaxC1=max(yMaxInd,yMaxVar)*1.1
		offsetY1=(yMaxC1-yMinC1)/30
	} else
	{
		# 1ere carte : individus
		xMinC1=xMinInd*1.1
		xMaxC1=xMaxInd*1.1
		yMinC1=yMinInd*1.1
		yMaxC1=yMaxInd*1.1
		
		# 2e carte : scoreName
		xMinC2=xMinVar*1.1
		xMaxC2=xMaxVar*1.1
		yMinC2=yMinVar*1.1
		yMaxC2=yMaxVar*1.1
		
		offsetY1=(yMaxC1-yMinC1)/30
		offsetY2=(yMaxC2-yMinC2)/30
		
		# Separation de l'ecran
		par(mfcol=c(1,2))
	}
	
	## Trace commun
	plot(NULL,pch=3,xlim=c(xMinC1,xMaxC1),ylim=c(yMinC1,yMaxC1),asp=1,xlab=x,ylab=y,axes=FALSE)        
	box()
	abline(v=0,lty=3)
	abline(h=0,lty=3)
	
	# Points
	points(ind,col=ic,pch=pchVect)
	
	# Individus
	if(indLabels==TRUE)
	{
		text(ind[,1],ind[,2]+offsetY1,labels=labelx,col=ic,cex=0.8)
	} 
	
	# ell
	if( !is.null(ell))
	{ 
		for(i in 1:nb.ind)
		{
			lines(ell[i,,],col=ic[i])
		}        
	}
	
	
		
	# scoreName supplementaires
	if(si)
	{
		for(i in 1:nb.ind)
		{
			if(!siLabels)
			{
				points(si[[i]],col=sic[i],pch=16)      
			}
			else
			{
				text(si[[i]],labels=rownames(si[[i]]),col=sic[i],pch=16) 
			}
		}
	}
		
	# Segments reliant individus identiques
	if (!is.null(seg))
	{
		for(i in 1:(nb.ind-1))
		{ 
			ind1=rownames(seg)[i]
			for(j in (i+1):(nb.ind))
			{
				ind2=rownames(seg)[j]
				if(seg[i,j]==TRUE)
				{
					segments(ind[rownames(ind)==ind1,1],ind[rownames(ind)==ind1,2],ind[rownames(ind)==ind2,1],ind[rownames(ind)==ind2,2])
				}
			}
		}
	}
	
	if (biplot)
	{
		# Biplot
		if(varsLabels==TRUE)
		{
			text(vars[,1]*epd,(vars[,2]*epd)+(sign(vars[,2])*offsetY1),labels=labely,col=varsColors,cex=0.8)
		}
		for(i in 1:nb.var)
		{
			#segments(vars[i,1]*epd,vars[i,2]*epd,0,0,col=varsColors[i])
			arrows(vars[i,1]*epd,vars[i,2]*epd,0,0,code = 1,length=0.1,col=varsColors[i])
		}
	} else
	{
		# 2 cartes
		
		plot(NULL,pch=3,xlim=c(-1,1),ylim=c(-1,1),asp=1,xlab=x,ylab=y,axes=FALSE)      
		# TODO : Cercle des correlations
		box()
		x.cercle <- seq(-1, 1, by = 0.01)
        y.cercle <- sqrt(1 - x.cercle^2)
        lines(x.cercle, y = y.cercle)
        lines(x.cercle, y = -y.cercle)

		for(i in 1:nb.var)
		{
		
			segments(vars[i,1],vars[i,2],0,0,col=varsColors[i])
			#arrows(vars[i,1],vars[i,2],0,0,code = 1,col=varsColors[i],length=1)
		}
		if(varsLabels==TRUE)
		{
			text(vars[,1],vars[,2]+(sign(vars[,2])*offsetY2),labels=labely,col=varsColors,cex=0.8)
		}
		abline(v=0,lty=3)
		abline(h=0,lty=3)
	}
	
	if(!biplot)
	{
		mtext("PCA",3,line=0.5,outer=T,cex=1.8,font=2)
		
	} else
	{
		mtext("PCA",3,line=1,outer=F,cex=1.8,font=2)
		
	}

}
