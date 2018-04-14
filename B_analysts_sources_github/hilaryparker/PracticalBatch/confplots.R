confplots<-function(filename,
               PAM=TRUE,mainttl){
	
	load(filename)
			   
	len<-length(test.pred.group)
	
	if(PAM==TRUE){
		for(i in 1:len){
			test.pred.group[[i]]<-as.numeric(test.pred.group[[i]])-1
		}
	}
	
	batchAtrue0pred0 <- c()
	batchAtrue1pred1 <- c()
	batchBtrue0pred0 <- c()
	batchBtrue1pred1 <- c()
	
	for(i in 1:len){
		batchAtrue0pred0[i]<-sum(testgrp[[i]]==0 & test.pred.group[[i]]==0 &
								 testbatch[[i]]=="A")/
					   sum(testgrp[[i]]==0 & testbatch[[i]]=="A")
					   
		batchAtrue1pred1[i]<-sum(testgrp[[i]]==1 & test.pred.group[[i]]==1 &
								 testbatch[[i]]=="A")/
					   sum(testgrp[[i]]==1 & testbatch[[i]]=="A")
					   
		batchBtrue0pred0[i]<-sum(testgrp[[i]]==0 & test.pred.group[[i]]==0 &
								 testbatch[[i]]=="B")/
					   sum(testgrp[[i]]==0 & testbatch[[i]]=="B")
					   
		batchBtrue1pred1[i]<-sum(testgrp[[i]]==1 & test.pred.group[[i]]==1 &
								 testbatch[[i]]=="B")/
					   sum(testgrp[[i]]==1 & testbatch[[i]]=="B")
	}
	
	
	
	indx<-c(rep(1,100),rep(2,100),3,rep(4,100),rep(5,100))

	
	res<-list(
		batchAtrue0pred0, #Batch does not agree
		batchAtrue1pred1,  #Batch agrees
		batchBtrue0pred0, #Batch agrees 
		batchBtrue1pred1  #Batch does not agree
	)
		
		
	mydarkblue   <-rgb(44, 105, 228, maxColorValue=255)
	mylightblue  <-rgb(151, 181, 241, maxColorValue=255)
	mydarkyel    <-rgb(255, 204, 102, maxColorValue=255)
	mylightyel   <-rgb(255, 240, 209, maxColorValue=255)

	
	boxplot(res,
			#main=paste("Batch 1 Results:",quant,"Probes Used"),
			#names=nms,
			col=c(mydarkblue,mylightblue,mydarkyel,mylightyel),
			varwidth=TRUE,
			outpch=20,
			outcex=0.5,
			lwd=2,
			medlwd=2,
			whisklty = "solid",
			xaxt='n',
			yaxt='n',
			ylim=c(0,1),
			main=mainttl,
			cex.main=2
			)
	#legend("bottomright",legend=c("Batch Conf.","Batch Not Conf."),fill=c("royalblue","gold"))
	#dev.off()
	
	axis(side=2, at=c(0,0.5,1), labels=c("0.0","0.5","1.0"), cex.axis=1.5, lwd.ticks=2)
}