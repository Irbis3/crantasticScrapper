## function for running confounding results
## No removing probes - this is just for confounding results
## if using RMA - need to upload CEL files!
## if using fRMA - can upload preprocessed data
## this is all in the sepdata code from Section 5!

# quant is the quantile of probes to be thrown out!

### CAN ONLY HAVE TSP OR PAM TRUE, NOT BOTH!! ###
conf_fun_reduced<-function(strt=1,fin=100,
			  bad.ps.obj=rma.bad.ps.1,
			  datA,datB,
			  filename,
              TSP=FALSE,PAM=TRUE,RMA=TRUE){

	print("data loaded")
	library(affy)

	if(RMA==FALSE){
		datA <- frmaA
		datB <- frmaB
	}
	
	#Housekeeping so that we can assess error rate later on
	testgrp <- list()          # the actual group
	test.pred.group <- list()  # the predicted group
	testbatch <- list()        # the batch
	fit <- list()
	
	for(z in strt:fin){
		print(z)
		
		# get correct bad probeset object #
		bad.ps<-bad.ps.obj[[z]]
		
		#sepdata has a unique index that identifies which arrays go into which group#
			# 1 = build 1
			# 2 = test 1
			# 3 = build 0
			# 4 = test 0		
				
		#Will use batch A for 0 (ER-) outcome, batch B for 1 (ER+) outcome#
		buildbatchA <- datA[,sepA[[z]]==3]
		buildbatchB <- datB[,sepB[[z]]==1]
		testbatchA <- datA[,sepA[[z]]==2 | sepA[[z]]==4]
		testbatchB <- datB[,sepB[[z]]==2 | sepB[[z]]==4]

		buildgrp <- c(grp[[1]][sepA[[z]]==3],grp[[2]][sepB[[z]]==1])
		testgrp[[z]] <- c(grp[[1]][sepA[[z]]==2 | sepA[[z]]==4],grp[[2]][sepB[[z]]==2 | sepB[[z]]==4])
		len <- length(testgrp[[z]])
		testbatch[[z]] <- c(rep("A",sum(sepA[[z]]==2 | sepA[[z]]==4)),rep("B",sum(sepB[[z]]==2 | sepB[[z]]==4)))
		test.pred.group[[z]] <- rep(NA,len)
		
		if(RMA==TRUE){
			build<-merge.AffyBatch(buildbatchA,buildbatchB)
			build<-exprs(rma(build))
			test<-merge.AffyBatch(testbatchA,testbatchB)
			test<-exprs(rma(test))
		}
		
		if(RMA==FALSE){
			build <- cbind(buildbatchA,buildbatchB)
			test<- cbind(testbatchA,testbatchB)
		}
		
		build <- build[!bad.ps,]
		test <- test[!bad.ps,]
	
		
		if(TSP==TRUE){
			library(tspair)	
			#Build/test TSP function:
			fit[[z]] <- tspcalc(build, buildgrp)
			for(i in 1:len){
				temp <- as.matrix(test[,i])
				test.pred.group[[z]][i] <- predict(fit[[z]],temp)
			}	
		}
		
		if(PAM==TRUE){
			library(pamr)
			fit[[z]] <- pamr.train(list(
                                   x=build,
                                   y=buildgrp))
								
			rev.fit <- rev(fit[[z]]$error)              
			pam.thresh <- fit[[z]]$threshold[length(rev.fit) -
												 which.min(rev.fit)+1]
           
			for(i in 1:len){
				temp <- as.matrix(test[,i])
				test.pred.group[[z]][i] <- pamr.predict(fit[[z]],temp,pam.thresh)
			}
		}
	}
	save(list=c("testbatch","testgrp","test.pred.group","fit"),
		 file=paste(filename,".RData",sep=""))
}