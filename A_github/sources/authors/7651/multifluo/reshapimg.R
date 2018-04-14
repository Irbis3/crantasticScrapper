reshapimg <-
function(zone.img,list.img,list.bool=NULL,reduction=TRUE,return.bool=FALSE,name.img=NULL)
{
	vecteur=list()
	if(is.null(names(list.img))&is.null(name.img)){names(list.img)=paste("Var",1:length(list.img),sep="")}
	if(is.null(names(list.img))&!is.null(name.img)){names(list.img)=name.img}
	#
	if(!is.null(list.bool))
	{	
		bool=list.bool[[1]]&!is.na(zone.img)
	}else
	{	
		bool=!is.na(zone.img)
	}

	for(i in 1:length(list.img))
	{
		if(sum(dim(list.img[[i]])==dim(zone.img))!=2){stop(paste(i, "element of list.img has not the same dimension than zone.img"))}
		vecteur[[i]]=as.vector(unlist(list.img[[i]]))
		bool=bool&!is.na(list.img[[i]])
		if(!is.null(list.bool[[i]]))
		{
				bool=bool&list.bool[[i]]
		}
	}
	
	new.vecteur=list()
	bool.vect=as.vector(unlist(bool))
	vect.zone=as.vector(unlist(zone.img))
	vect.zone.without.NA=vect.zone[bool.vect]
	for(i in 1:length(list.img))
	{
		new.vecteur[[i]]=vecteur[[i]][bool.vect]
		if(reduction){new.vecteur[[i]]=scale(vecteur[[i]][bool.vect])}
	}
	# on selectionne unicuement les endroits ou on a plus de 100 counts et un To<10 (sinon pb dans scaling)
	
	# vecPos2=vecPos[boolean]
	# vecCount2=vecCount[boolean]
	# vecTo2=vecTo[boolean]
	# vecGP2=vecGP[boolean]

	# # on scale (chaque variable doit avoir le meme poids)
	# if(reduction)
	# {
		# vecCount2=scale(vecCount2)
		# vecGP2=scale(vecGP2)
		# vecTo2=scale(vecTo2)
	# }
	vecPix=paste("P",1:length(new.vecteur[[1]]),sep="")
	dataSP=data.frame(new.vecteur)
	colnames(dataSP)=names(list.img)
	dataSP[,"zone"]=vect.zone.without.NA
	dataSP[,"pixel"]=vecPix
	if(return.bool)
	{
		res=list(data=dataSP,boolean=bool.vect)
	}
	else
	{	
		res=dataSP
	}
	return(res)
}
