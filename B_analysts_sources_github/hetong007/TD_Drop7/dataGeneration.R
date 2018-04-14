getProp=function(board)
{
	ans=board
	for (i in 1:7)
		for (j in 1:7)
		{
			if (!is.finite(board[i,j]))
				ans[i,j]=0
			else if (board[i,j]<0)
				ans[i,j]=-1
			else 
				ans[i,j]=1
		}
	return(ans)
}

dataGen=function(recData,model=NULL)
{
	scoreTimer=1
	board=init()
	
	totalScore=0
	tmp=elimFall(board,totalScore,scoreTimer)
	board=tmp[[1]]
	
	prevScore=0
	fall=0
	
	timer=5
	gameEnd=nofalljudge(board)
	
	#Sys.sleep(5)
	while (!gameEnd)
	{
		timer=timer-1
		
		fall=sample(1:7,1)
		tcol=chooseCol(board,fall,model.rf)
		board=throwFall(board,tcol,fall)
		scoreTimer=1
		tmp=elimFall(board,totalScore,scoreTimer)
		board=tmp[[1]]
		totalScore=tmp[[2]]

		gameEnd=(gameEnd || nofalljudge(board))
		if (!gameEnd && timer==0)
		{
			gameEnd=(gameEnd || overjudge(board))
			if (!gameEnd)
			{
				board=roundAdd(board)
				totalScore=totalScore+17000
				gameEnd=(gameEnd || nofalljudge(board))
				timer=5;
			}
		}
		tmp=elimFall(board,totalScore,scoreTimer)
		board=tmp[[1]]
		totalScore=tmp[[2]]
		gameEnd=(gameEnd || nofalljudge(board))
		
		if (recData[nrow(recData),1]>0)
			return(recData)
		
		incScore=totalScore-prevScore
		prevScore=totalScore
		straightBoard=as.vector(board)
		straightProp=as.vector(getProp(board))
		
		ind=which(recData[,1]==-1)[1]
		recData[ind,]=c(incScore,fall,tcol,straightBoard,straightProp)
		
	}
	return(recData)
}

recData=matrix(rep(-1,101*10000),ncol=101)
for (i in 1:1000)
{
	recData=dataGen(recData)
	ind=which(recData[,1]==-1)[1]
	show(c(i,ind))
	if (is.na(ind))
		break;
}

write.csv(recData,file="rawRec.10000.csv",row.names=F)
