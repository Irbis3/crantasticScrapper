survQuantile <-
function(func,probs){

	quantiles <- NULL

	for(p in probs){

		y <- 1-p

		aux <- func[func[,2]==y,1]

		if(length(aux)!=0){

			x <- aux[1]

		}else{

			aux1 <- func[func[,2]<=y,]
			aux2 <- func[func[,2]>=y,]
		

			if(length(aux1)==0 || length(aux2)==0){ 
				x <- NA
			}
			else{
				if(length(aux1)/2==1){
					x1 <- aux1[1]
					y1 <- aux1[2] 
				}else{
					x1 <- aux1[1,1]
					y1 <- aux1[1,2] 
				}

				last <- length(aux2)/2
				if(last==1){
					x2 <- aux2[1]
					y2 <- aux2[2]
				}else{ 
					x2 <- aux2[last,1]
					y2 <- aux2[last,2]
				}
	
				x <- ((y-y1)/(y2-y1))*(x2-x1) + x1
			}
		}

		quantiles <- append(quantiles,x)
	}

	return(quantiles)
}

