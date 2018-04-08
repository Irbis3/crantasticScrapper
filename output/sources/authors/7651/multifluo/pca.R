pca <-
function(data,zone="zone",pixel=NULL)
{ 
	scaleUnit=FALSE
   
	# products=levels(data$ProductCode)
	# subjects=levels(data$SubjectCode)
	# nbSubjects=length(subjects)
	# nbProducts=length(products)
	if(!zone %in% colnames(data)){stop("zone is not in colnames(data)")}
	if(is.null(pixel)){attributs=colnames(data)[colnames(data)!=zone]}else{attributs=colnames(data)[colnames(data)!=zone&colnames(data)!=pixel]}
	
	nbAttributes=length(attributs)
	
	# Tableau produits*attributs
	
	CenteredProductMeansTable=data.frame(NULL)
	CenteredProductMeansTable=aggregate(data[,attributs[1]],by=list(data[,zone]),FUN="mean",na.rm=TRUE)
	for(i in 2:length(attributs))
	{	
		 CenteredProductMeansTable[,attributs[i]]=aggregate(data[,attributs[i]],by=list(data[,zone]),FUN="mean",na.rm=TRUE)[,2]
	}
	
	colnames(CenteredProductMeansTable)=c("zone",attributs)
	moyennes=apply(CenteredProductMeansTable[,attributs],2,FUN="mean",na.rm=TRUE)
	ecartTypes=moyennes
	for(i in 1:length(attributs))
	{
		ecartTypes[i]=sqrt(sum((CenteredProductMeansTable[,attributs[i]]-moyennes[attributs[i]])^2))
	}
	
	CenteredProductMeansTable[,attributs]=apply(CenteredProductMeansTable[,attributs],2,"scale",center=TRUE,scale=scaleUnit)
	print(CenteredProductMeansTable)
	# Tests conditions ACP
	if (length(which(is.na(CenteredProductMeansTable))>0))
	{
		stop("NA in matrix.")
	}
	if(nbAttributes < 2)
	{
		stop("Insufficient number of attributes.")
	}
	
	# Calcul ACP
	matBmod=CenteredProductMeansTable[,attributs]
	rownames(matBmod)=rownames(CenteredProductMeansTable[,"zone"])
	resSvd=svd(matBmod)
	
	# Valeurs et vecteurs propres
	eigVal=(resSvd$d)^2
	eigVec=resSvd$v

	# Matrice des individus et scoreName de l'ACP
	individuals=as.matrix(CenteredProductMeansTable[,attributs])%*%eigVec
	variables=cor(as.matrix(CenteredProductMeansTable[,attributs]),individuals)
	rownames(individuals)=CenteredProductMeansTable[,"zone"]
	
	# Dimensions significatives : contributions > 1/nb variables
	p=1/nbAttributes
	contrib=eigVal/sum(eigVal)
	NbDimSig=length(contrib[contrib>p])
	
	suppIndividuals=NULL
	# Individus supplementaires
	if(!is.null(pixel))
	{
	
		if(pixel %in% colnames(data))
		{
			CenteredProductSubjectMeanTable=data.frame(NULL)
			CenteredProductSubjectMeanTable=aggregate(data[,attributs[1]],by=list(data[,zone],data[,pixel]),FUN="mean",na.rm=TRUE)
			for(i in 2:length(attributs))
			{	
				CenteredProductSubjectMeanTable[,attributs[i]]=aggregate(data[,attributs[i]],by=list(data[,zone],data[,pixel]),FUN="mean",na.rm=TRUE)[,3]
			}
			colnames(CenteredProductSubjectMeanTable)=c("zone","pixel",attributs)
			#CenteredProductSubjectMeanTable[,attributs]=apply(CenteredProductSubjectMeanTable[,attributs],2,"scale",center=TRUE,scale=FALSE)
			
			CenteredProductSubjectMeanTable2=CenteredProductSubjectMeanTable
			for(i in 1:length(attributs))
			{
				CenteredProductSubjectMeanTable2[,attributs[i]]=CenteredProductSubjectMeanTable[,attributs[i]]-moyennes[attributs[i]] # ici 
			}
			colnames(CenteredProductSubjectMeanTable2)=c("zone","pixel",attributs)
		
			suppIndividuals=CenteredProductSubjectMeanTable2
				
			
		}
		else
		{
			stop("pixel is not in colnames(data)")
		}
		
	}
	#manovaStats=summary(manova(data[,attributs]~as.factor(data[,zone])))
           
	resultList=list(B=matBmod, IndSup=suppIndividuals, EigenVectors=eigVec, EigenValues=eigVal, IndivCoord=individuals, VarCoord=variables, NbdimSig=NbDimSig)
	return(resultList) 
}
