difftable <-
function(mat,vep=NULL,axes=c(1,2),var.col=NULL,trt="zone",test="hotelling")
{
	# mat est la matrice produit*sujet
	
	prod=levels(as.factor(mat[,trt]))
	nb.prod=length(prod)
	tab=matrix(NA,nb.prod,nb.prod) 

	if(is.null(var.col)){var.col=which(!colnames(mat)==trt)}
	for(i in 1:nb.prod)
	{
		for(j in 1:nb.prod)
		{ 
			if(j!=i)
			{
				if(j<i)
				{
					if(!is.null(vep))
					{ 
						prodi=as.matrix(mat[mat[,trt]==prod[i],var.col])%*%vep[,axes]  
						prodj=as.matrix(mat[mat[,trt]==prod[j],var.col])%*%vep[,axes]             
					}
					if(is.null(vep))
					{           
						prodi=mat[mat[,trt]==prod[i],var.col]  
						prodj=mat[mat[,trt]==prod[j],var.col]
					} 
					if(test=="hotelling")
					{
						#L=HotellingTest(prodi,prodj)
						L=hotelling.test(matx=prodi, maty=prodj)	
						pval=L$p.value	
					}
					if(test=="sri")
					{
						L=sri.test(prodi,prodj)
						pval=L$p.value
					}
					tab[i,j]=pval
					tab[j,i]=pval        
				} 
			} else
			{
				tab[i,j]=1
			}
		}
	}
	colnames(tab)=prod
	rownames(tab)=prod
	return(tab)
}
