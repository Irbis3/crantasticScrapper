#The very simple method: randomly choose column		
chooseCol=function(board,fall,model=NULL)
{
	n=sample(1:7,7)
	for (i in 1:7)
		if (sum(is.finite(board[,n[i]]))<7)
			return(n[i])
	return(-1)
}

