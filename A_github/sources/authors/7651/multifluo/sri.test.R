sri.test <-
function(matx,maty)
{
	nb.suj.x=dim(matx)[1]
	nb.suj.y=dim(maty)[1]
	nb.att=dim(matx)[2]
	moyx=apply(matx,2,mean,na.rm=TRUE)
	moyy=apply(maty,2,mean,na.rm=TRUE)
	resx=matx
	resy=maty
	for(i in 1:nb.suj.x)
	{
		resx[i,]=matx[i,]-moyx	
	}
	for(i in 1:nb.suj.y)
	{
		resy[i,]=maty[i,]-moyy	
	}
	mat.resx=as.matrix(resx)
	mat.resy=as.matrix(resy)

	W1=t(mat.resx)%*%mat.resx
	W2=t(mat.resy)%*%mat.resy	

	W=(W1+W2)/(nb.suj.x+nb.suj.y-2)
	D=diag(diag(W))
	Dinv=solve(D)
	n=(nb.suj.x+nb.suj.y-2)
	p=nb.att
	R=sqrt(Dinv)%*%W%*%sqrt(Dinv)
	cpn=1+sum(diag(R%*%R))/(p^(3/2))
		
		T2=((nb.suj.x*nb.suj.y)/(nb.suj.x+nb.suj.y))*(t(moyx-moyy)%*%Dinv%*%(moyx-moyy))
		t2=T2[1,1]
		t.sri=(t2-n*p/(n-2))/sqrt(2*((sum(diag(R%*%R))-p^2/n)*cpn))
	#	stat=t2*(2*nb.suj-nb.att-1)/((2*nb.suj-2)*nb.att)
		
		# p.value=pf(stat,df1=nb.att,df2=nb.suj.x+nb.suj.y-1-nb.att,lower.tail=FALSE)	
	p.value=pnorm(t.sri,lower.tail=FALSE)	
	
	L=list(p.value=p.value,stat=t.sri)
	return(L)
}
