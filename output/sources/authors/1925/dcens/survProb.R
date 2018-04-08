survProb <-
function(func,times){

	probs <- NULL

	for(t in times){

		aux <- func[func[,1]<=t,]

		last <- length(aux)/2

		y <- ifelse(last==1,aux[2],aux[last,2])
	
		probs <- append(probs,1-y)
	}

	return(probs)
}

