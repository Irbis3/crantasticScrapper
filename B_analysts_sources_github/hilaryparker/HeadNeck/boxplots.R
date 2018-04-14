setwd("/home/bst/student/hiparker/HeadNeck")
library(ProjectTemplate)
load.project()

y<-list(predictor_results$none.out,predictor_results$exact.out,predictor_results$fast.out)

round(mean(predictor_results$none.out),2)
round(mean(predictor_results$dbonly.out),2)
round(mean(predictor_results$dbfsva.out),2)

y1<-predictor_results$dbonly.out-predictor_results$none.out
t.test(y1)
boxplot(y1)

y2<-predictor_results$dbfsva.out-predictor_results$none.out
t.test(y2)
boxplot(y2)


y3<-predictor_results$dbfsva.out-predictor_results$dbonly.out
t.test(y2)
boxplot(y2)


cols<-brewer.pal(3, "Dark2")

pdf(file='test.pdf')
boxplot(y,
		las=1,
		col=cols,
		outpch=20,
		outcex=0.5,
		lwd=2,
		medlwd=2,
		whisklty = "solid",
		xaxt='n',
		cex.axis=1.3)
		
	axis(side=1, at = c(1,2,3), 
	     labels=c("None","Exact fSVA","Fast fSVA"), lwd=2, cex.axis=1.3)
		
		
dev.off()
		
	boxplot(y,
		las=1,
		col=col,
		varwidth=TRUE,
		outpch=20,
		outcex=0.5,
		lwd=2,
		medlwd=2,
		whisklty = "solid",
		xaxt='n',
		cex.axis=1.3)
		
		
	abline(v=10,lty=3,lwd=2)
	#abline(h=hl,lty=2)

	axis(side=3, at = c(5,15), labels=c("RMA","fRMA"), tick=FALSE, lwd=2, cex.axis=1.3)
	axis(side=1, at = c(-0.75,2,5,8,12,15,18,20.5), 
	     labels=c("",rep(c("LASSO","TSP","PAM"),2),""), lwd=2, cex.axis=1.3)


	legend("bottomright",c("Within","Between","Pooled"),col=c(1:3),
			pch=c(15,15,15),
			pt.cex=2,
			lty=c(0,0,0),
			lwd=c(0,0,0),
			cex=1.3)
			
	legend("bottomright",c("Within","Between","Pooled"),col="black",
			pch=c(22,22,22),
			pt.cex=2,
			lwd=c(2,2,2),
			lty=c(0,0,0),
			cex=1.3)
			
	legend("bottomleft",paste(nrep,"Iterations"),cex=1.3)
