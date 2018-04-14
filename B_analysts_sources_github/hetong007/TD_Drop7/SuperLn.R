#dat=getData()
dat=read.csv("rawRec.10000.csv",header=F)
for (i in 1:101)
	dat[,i]=as.numeric(dat[,i])
dat[,1]=log(dat[,1])

#RandomForest Model
require(randomForest)
system.time((model.rf=randomForest(V1~.,data=dat[1:500,])))#6 secs
system.time((model.rf=randomForest(V1~.,data=dat[1:2000,])))#54 secs
system.time((model.rf=randomForest(V1~.,data=dat[1:5000,])))#193 secs
system.time((model.rf=randomForest(V1~.,data=dat)))#193 secs


#NeuralNet Model
chr=rep("V",100)
for (i in 1:100)
	chr[i]=paste(c("V",as.character(i+1)),collapse="")
chr=paste(chr,collapse="+")
fml=as.formula(paste("V1~",chr),collapse="")
	
	
require(neuralnet)
system.time((model.nn=neuralnet(fml,data=dat[1:500,],size=7)))
system.time((model.nn2=neuralnet(fml,data=dat,hidden=2)))#646.29
system.time((model.nn3=neuralnet(fml,data=dat,hidden=3)))#?




chooseCol=function(board,fall,model)
{
	propBoard=getProp(board)
	straightBoard=as.vector(board)
	straightBoard[which(!is.finite(straightBoard))]=0
	
	straightProp=as.vector(propBoard)
	sitDat=as.data.frame(t(matrix(c(fall,1,straightBoard,straightProp))))
	for (i in 2:7)
		sitDat=rbind(sitDat,c(fall,i,straightBoard,straightProp))
	names(sitDat)=names(dat)[2:101]
	
	pred=predict(model,newdata=sitDat)
	
	ind=sort(pred,decreasing=T)
	for (i in 1:7)
		if (sum(is.finite(board[,ind[i]]))<7)
			return(ind[i])
	return(-1)
}

chooseCol(board,5,model.rf)
