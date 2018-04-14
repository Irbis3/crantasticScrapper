find.colors <-
function(vect,color=NULL)
{
	vectColor=vect
	premiereLettre=levels(factor(substr(vect,1,1)))
	if(is.null(color)||length(color)!=length(premiereLettre)){color=rainbow(length(premiereLettre))}
	for(i in 1:length(vect))
	{
		nom=vect[i]		
		# if(temp)
		# {
			# derniereLettre=substr(nom,nchar(nom)-1,nchar(nom))
			# if(derniereLettre=="10"){vectColor[i]=color[1]}
			# if(derniereLettre=="23"){vectColor[i]=color[2]}
			# if(derniereLettre=="37"){vectColor[i]=color[3]}
		# }
		# if(!temp)
		#{
			 premiereLettrei=substr(nom,1,1)
			# if(!pch)
			# {
				for(j in 1:length(premiereLettre))
				{
					if(premiereLettrei==premiereLettre[j])
					{	
						vectColor[i]=color[j]
					}
				}
			# }
			# if(pch)
			# {
				# if(premiereLettrei=="F"){vectColor[i]=color[1]}
				# if(premiereLettrei=="M"){vectColor[i]=color[2]}
				# if(premiereLettrei=="T"){vectColor[i]=color[3]}
			# }	
		# }		
	}
	return(vectColor)

}
