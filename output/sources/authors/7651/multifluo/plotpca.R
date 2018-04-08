plotpca <-
function(x,map="b",n="a", l=0.05,col=NULL,calc="Chi",epd=NULL,et="bar",dif="h",dax="all")
{ #gerer isup ou si
	fileName = "PCA"
	main="PCA"
	productLabel=TRUE
	variablesLabels=TRUE
	if(dif=="h"){tdif="hotelling";crc="n"}
	if(dif=="hc"){tdif="hotelling";crc="bonf"}
	if(dif=="sc"){tdif="sri";crc="bonf"}
	if(dif=="s"){tdif="sri";crc="n"}
	if(dif=="n"){link=FALSE}else{link=TRUE}
	if(!map%in%c("bil","bip","b","til","t","tip")){map="b";warning("Wrong map name. Please choose between 'b','bil','bip','t','til',tip'.")}

	if(map=="bil"){		si=TRUE;sindLabels=TRUE;biplot=TRUE}
	if(map=="bip"){		si=TRUE;sindLabels=FALSE;biplot=TRUE}
	if(map=="b"){		si=FALSE;sindLabels=FALSE;biplot=TRUE}
	if(map=="til"){		si=TRUE;sindLabels=TRUE;biplot=FALSE}
	if(map=="ti"){		si=TRUE;sindLabels=FALSE;biplot=FALSE}
	if(map=="t"){		si=FALSE;sindLabels=FALSE;biplot=FALSE}	

	if(!et %in% c("bar","ind")){et="n"}
	res.PlotPCA=list()
	confInt=1-l

	# Transformation des individus et scoreName pour le biplot
	if(biplot) # l=1
	{
		matsvd=svd(as.matrix(x$B))
		U=(matsvd$u)
		D=diag(matsvd$d)
		V=matsvd$v
		ud=U%*%D
		x$IndivCoord[,1]=ud[,1]
		x$IndivCoord[,2]=ud[,2]
		x$VarCoord[,1]=V[,1] # loadings
		x$VarCoord[,2]=V[,2]	
		biplot=TRUE 
	}
	title=main
	res.PlotPCA[["IndivCoord"]]=x$IndivCoord
	res.PlotPCA[["VarCoord"]]=x$VarCoord
	if(link & is.null(x$IndSup))
	{
		warning("Please enter a x with ellipse calculation or panelists for the calculation of the test (Supplementary individuals are required). The test is not run ! and not significant zones would not be linked")
		link=FALSE
	}
	if (link)
	{
			#hot=hotellingTable(matCva=x$IndSup,productName="zone",vep=x$EigenVectors[,1:n],axes=c(1:n)) 
		if(dax=="all")
		{
			hot=difftable(mat=x$IndSup[,-which(colnames(x$IndSup)=="pixel")],trt="zone",vep=NULL,axes=c(1:2),var.col=NULL,test=tdif) 
			
		}
		if(dax=="sig")
		{
			x$IndSup[,-which(colnames(x$IndSup)=="pixel")]
			hot=difftable(mat=x$IndSup[,-which(colnames(x$IndSup)=="pixel")],trt="zone",vep=x$EigenVectors,axes=c(1:x$NbDimSig),var.col=NULL,test=tdif) 
		}
		if(is.numeric(dax))
		{
			hot=difftable(mat=x$IndSup[,-which(colnames(x$IndSup)=="pixel")],trt="zone",vep=x$EigenVectors,axes=dax,var.col=NULL,test=tdif) 

		}
		if(crc!="bonf")
		{
			individualsSegments=hot>1-confInt
		}
		if(crc=="bonf")
		{
			n.comparaisons=dim(x$IndivCoord)[1]
			individualsSegments=hot>1-confInt/(n.comparaisons*(n.comparaisons-1)/2)
		}
		
	} else 
	{
		individualsSegments=NULL
	}		
	# Un biplot par combinaison d'axes
	if (n=="a"||n=="a")
	{
		n=max(2,x$NbDimSig)
	} 
	axes=combn(n,2)
	
	if(is.null(ncol(axes)))
	{
		axes=matrix(c(1,2),2,1)
	}
	res.PlotPCA[["axes"]]=axes
	for (i in 1:ncol(axes))
	{
		main=title
		axe1=axes[1,i]
		axe2=axes[2,i] 
		inertie1=x$EigenValues[axe1]
		inertie2=x$EigenValues[axe2]
		inertieCumul=round(100*(inertie1+inertie2)/sum(x$EigenValues),digits=2)
		res.PlotPCA[["cumulInertia"]]=	inertieCumul
		# Individus supp
		suppIndividuals = NULL
		if (et!="n" | si==FALSE)
		{			
			suppIndividuals=x$IndSup
		} 
		if(is.null(x$IndSup))
		{
			suppIndividuals=NULL
			et="n"
			panellits="n"
			if(is.null(x$IndSup)){print("No supplementary individuals in x")}	
		}
	
		# Ellipses
		individualsEllipses=NULL
		panelistCoord=NULL
		if (et!="n" | si==FALSE)
		{
			calculationsEllipse=calc.ell(sup.ind=suppIndividuals,vep=x$EigenVectors,axes=c(axe1,axe2), conf=confInt,ell.t=et,calc=calc)
			panelistCoord=calculationsEllipse[[1]]
			individualsEllipses= calculationsEllipse[[2]] 
			res.PlotPCA[["ellipses"]]=calculationsEllipse
		}  
					
		# variables required to plot
		if(is.null(col))
		{
			col=rep("blue",nrow(x$IndivCoord))
		}
			
		# Panelistes
		# suppIndividualsLabels=FALSE
		# suppIndividualsToPlot=FALSE
	
		# if (isup!="n")
		# {
			# suppIndividualsToPlot=TRUE
			# suppIndividualsLabels=FALSE
			# if(isup=="label")
			# {
				# suppIndividualsLabels=TRUE
			# }
			
		# }

		main=paste(main," (",inertieCumul,"%)",sep="")	
		xlab=paste("Dim. ",axe1," (",round(inertie1*100/sum(x$EigenValues),digits=2),"%)",sep="")
		ylab=paste("Dim. ",axe2," (",round(inertie2*100/sum(x$EigenValues),digits=2),"%)",sep="")
		
		fun=call("intpca",vars=x$VarCoord[,c(axe1,axe2)],ind=x$IndivCoord[,c(axe1,axe2)],map=map,si=panelistCoord,ic=col,sic=NULL,ell=individualsEllipses,seg=individualsSegments,epd=epd,x=xlab,y=ylab)

		
		#fun=call("internal.pca",variables=x$VarCoord[,c(axe1,axe2)], individuals=x$IndivCoord[,c(axe1,axe2)], suppIndividuals=panelistCoord, 
			#suppIndividualsToPlot=suppIndividualsToPlot, biplot=biplot, individualscol=col, suppIndividualscol=NULL,
			#variablesLabels=variablesLabels,individualsSegments=individualsSegments, individualsLabels=productLabel, suppIndividualsLabels=suppIndividualsLabels, individualsEllipses=individualsEllipses, revX=revX,revY=revY,
			#xlab=xlab, ylab=ylab, epd=epd, mainTitle=main,cex=1.5,pchVect=pchVect)
	
		if(biplot)
		{
		
			width=7
		} else 
		{
			width=14
		}
		lapply(X=c(fun), FUN = eval) 
		generic.plot(type = "R", name = paste("Axes",axe1,"&",axe2), CALLFUN = fun, width = width, height = 7)    
		# PlotPCACVA(variables=x$VarCoord[,c(axe1,axe2)], individuals=x$IndivCoord[,c(axe1,axe2)], suppIndividuals=panelistCoord, 
		# suppIndividualsToPlot=suppIndividualsToPlot, biplot=biplot, variablescol=NULL, individualscol=col, suppIndividualscol=NULL,
		# variablesLabels=TRUE, individualsLabels=TRUE, suppIndividualsLabels=suppIndividualsLabels, individualsEllipses=individualsEllipses, revX=revX,revY=revY,
		# xlab=xlab, ylab=ylab, epd=NULL, mainTitle=main)		 
		
	}
	return(res.PlotPCA)
}
