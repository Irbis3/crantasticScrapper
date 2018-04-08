hotelling.test <-
function(matx,maty)
{
	egalite=function(x,y)
	{
		bool=TRUE
		for(i in 1:length(x)[1]){	if(x[i]!=y[i]){ bool=FALSE}}
		return(bool)
	}
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
	if(det(W)>1e-8)
	{
		Winv=solve(W)
		T2=((nb.suj.x*nb.suj.y)/(nb.suj.x+nb.suj.y))*(t(moyx-moyy)%*%Winv%*%(moyx-moyy))
		
		t2=T2[1,1]
		stat=t2*(nb.suj.x+nb.suj.y-nb.att-1)/((nb.suj.x+nb.suj.y-2)*nb.att)
		p.value=pf(stat,df1=nb.att,df2=nb.suj.x+nb.suj.y-1-nb.att,lower.tail=FALSE)	
	}
	else
	{
		print("Not invertible matrix : maybe you have more variables than observations ? In this case, run a sri.test")
		if(egalite(moyx,moyy))
		{
			p.value=NA
			stat=NA
		}
		else
		{
			p.value=NA
			stat=NA
		}
	}
	L=list(p.value=p.value,stat=stat)
	return(L)
}
