calc.ell <-
function(sup.ind,vep,axes=c(1,2),conf=0.9,ell.t="bar",trt="zone",sup="pixel",calc="Chi")
{ 
	listeNA=apply(is.na(sup.ind),1,"sum")
	if(length(which(listeNA>0))>0)
	{
		sup.ind=sup.ind[-which(listeNA>0),]
	}
	products=levels(as.factor(as.character(sup.ind[,trt])))
	nb.prod=length(products)
	suj=levels(as.factor(sup.ind[,sup]))
	nb.suj=length(suj)
	ell=array(dim=c(nb.prod,100,2))
	ell2=array(dim=c(nb.prod,100,2))
	coord.pts=list()
	attributs=colnames(sup.ind)[-c(1,2)]
	nbAttributes=length(attributs)
	ax1=axes[1]
	ax2=axes[2]
	coorCentre=data.frame()
	for(i in 1:nb.prod)
	{     

		indiv.sup=as.matrix(sup.ind[sup.ind[,trt]==products[i],which(!colnames(sup.ind)%in%c(trt,sup))])
		rownames(indiv.sup)=sup.ind[sup.ind[,trt]==products[i],sup]
		coord.indiv.sup=indiv.sup%*%vep
		rownames(coord.indiv.sup)=rownames(indiv.sup)
		coord.mean=apply(indiv.sup,2,"mean")%*%vep
		coorCentre[i,1]=coord.mean[,ax1]
		coorCentre[i,2]=coord.mean[,ax2]
		coorCentre[i,trt]=products[i]
		coord.pts[[i]]=coord.indiv.sup[,c(ax1,ax2)] 
		
		ddlN=length(coord.pts[[i]][,1])
		matCov=cov(coord.pts[[i]])
		matCov2=matCov/dim(coord.indiv.sup)[1]
		if(ell.t!="ind"){covarianceMatrix=matCov2}
		if(ell.t=="ind"){covarianceMatrix=matCov}
		quant=sqrt(2*nb.suj*qf(conf,2,nb.suj-2)/(nb.suj-2))
		if(calc=="Chi"){quant=sqrt(qchisq(conf,2))}
		if(calc=="F"){quant=sqrt(2*nb.suj*qf(conf,2,nb.suj-2)/(nb.suj-2))}
		if(calc=="Sas"){quant=sqrt((2*nb.suj*qf(conf,2,nb.suj-2)/(nb.suj-2))*(nb.suj-1)/nb.suj)}
			
		ell[i,,1]=ellipse::ellipse(x=covarianceMatrix,centre=c(coorCentre[i,1],coorCentre[i,2]),level=conf,t=quant)[,1]
		ell[i,,2]=ellipse::ellipse(x=covarianceMatrix,centre=c(coorCentre[i,1],coorCentre[i,2]),level=conf,t=quant)[,2] 	
		ell[i,,1]=ellipse::ellipse(covarianceMatrix,centre=c(coorCentre[i,1],coorCentre[i,2]),level=conf,t=quant)[,1]
		ell[i,,2]=ellipse::ellipse(covarianceMatrix,centre=c(coorCentre[i,1],coorCentre[i,2]),level=conf,t=quant)[,2] 
	}

	L=list(coord.pts,ell,coorCentre)
	return(L)
}
