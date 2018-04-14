erase <-
function(zone.img,img,interact=TRUE,shape="rect",pts=NULL)
{
	zone.to.suppress=create.zones(img=img,n.zones=1,shape=shape,name.zones=c("to.suppress"),interact=interact,pts=pts)
	matPos=img.zone(zone=zone.to.suppress,d=NULL,mask=NULL,edge.only=FALSE,wo.edge=FALSE,graph=FALSE,img=img,lim=30)	
	zone.img[matPos=="to.suppress"]=NA
	return(zone.img)
}
