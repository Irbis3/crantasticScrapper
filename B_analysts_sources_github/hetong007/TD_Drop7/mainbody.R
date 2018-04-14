drop7=function(dat,trainingLines=500,model=NULL)
{
	if (FALSE)
	{
		if (is.null(model))
		{
			show("trainingData")
			require(randomForest)
			n=nrow(dat)
			ind=sample(1:n,trainingLines)
			show(system.time((model.rf=randomForest(V1~.,data=dat[ind,]))))
		}
		else
			model.rf=model
	}
	board=init()
	totalScore=0
    records = list(board)
    recs = 1
	
	scoreTimer=1
	tmp=elimFall(board,totalScore,scoreTimer)
	board=tmp[[1]]
	timer=5
	#show(board)
	gameEnd=nofalljudge(board)
	
	#Sys.sleep(5)
	while (!gameEnd)
	{
		timer=timer-1
		fall=sample(1:7,1)
        
		tcol=chooseCol(board,fall,model.rf)#The Intellegence
        
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
        recs = recs+1
        records[[recs]] = board
		#show(board)
		#Sys.sleep(5)
	}
	return(list(totalScore,records))
	#show(totalScore)
	#show(board)
	#show("game over!")
}

drop7()

a=NULL
for (i in 1:100)
{
	a=c(a,drop7(dat=dat,model=model.rf))
	show(i)
}
