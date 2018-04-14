interieur.para <-
function(x,y,xOrigine,yOrigine,xA,yA,xB=NA,yB=NA,shape="para")
{
#x,y: point dont la position est a determiner
# xOrigine,yOrigine : point "d angle" du parallelogramme x0
# xA,yA et xB yB : point du bord du parallelogramme
	interieur=FALSE
	if(shape=="para")
	{
		if(yA!=yOrigine)
		{
			mu=((x-xOrigine)*(yA-yOrigine)-(y-yOrigine)*(xA-xOrigine))/((xB-xOrigine)*(yA-yOrigine)-(yB-yOrigine)*(xA-xOrigine))
			lambda=((y-yOrigine)-mu*(yB-yOrigine))/(yA-yOrigine)
		}
		if(yA==yOrigine)
		{
			mu=(y-yOrigine)/(yB-yOrigine)
			lambda=((x-xOrigine)-mu*(xB-xOrigine))/(xA-xOrigine)
		}
		if(mu>=0&mu<=1&lambda>=0&lambda<=1){interieur=TRUE}
	}
	if(shape=="rect")
	{
		xMin=min(xOrigine,xA,xB,na.rm=TRUE)
		xMax=max(xOrigine,xA,xB,na.rm=TRUE)
		yMin=min(yOrigine,yA,yB,na.rm=TRUE)
		yMax=max(yOrigine,yA,yB,na.rm=TRUE)
		
		if(xMin>x & x<xMax & yMin<y &y<yMax)
		{
			interieur=TRUE
		}
	}
	if(shape=="cercle")
	{
		
		R=sqrt((xA-xOrigine)^2+(yA-yOrigine)^2)
		if((x-xOrigine)^2+(y-yOrigine)^2<R^2){interieur=TRUE}
	}
	
	
	return(interieur)
}
