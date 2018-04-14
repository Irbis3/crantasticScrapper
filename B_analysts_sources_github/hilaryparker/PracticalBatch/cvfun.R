cvfun <- function(filename,nrep,nbatch,
				  celA,
				  celB,
				  grp,
				  celp,pooledgrp){

	dimA<-length(celA)
	dimB<-length(celB)
	
	## frma preprocessing - can do all at once! ##
	library(frma)
	frmap<-exprs(frma(celp))
	frmaA<-frmap[,1:dimA]
	frmaB<-frmap[,(dimA+1):(dimA+dimB)]
	detach(package:frma)


	# some housekeeping for future use... #
	narrays <- rep(0,nbatch)
	for(i in 1:nbatch){
		narrays[i] <- length(grp[[i]])
	}
	pooled.narrays <- length(pooledgrp)

	## samplesize = even number size of smallest batch ##
	samplesize <- min(narrays)
	if(samplesize %% 2 == 1){
		samplesize <- samplesize - 1
	}

	# prep lists for results #
	rma.tsp.result <- list()
	frma.tsp.result <- list()
	rma.lasso.result <- list()
	frma.lasso.result <- list()

	rma.pam.result <- list()
	rma.pam.numgenes <- list()
	frma.pam.result <- list()
	frma.pam.numgenes <- list()
    
	pooled.rma.tsp.result <- c()
	pooled.rma.pam.result <- c()
	pooled.rma.lasso.result <- c()
	pooled.rma.pam.numgenes <- c()
    
	pooled.frma.tsp.result <- c()
	pooled.frma.pam.result <- c()
	pooled.frma.lasso.result <- c()
	pooled.frma.pam.numgenes <- c()
    		

	# will use these to determine cutoffs for test versus build sets #
	n1 <- samplesize/2

    
    # get a list with 1:narrays for each of the batches #
	index <- list()
	for(t in 1:nbatch){
		index[[t]] <- 1:narrays[t]
	}
	pooled.index <- 1:pooled.narrays
	
	
	#Find proportion of grp1 in each batch
	props<-c()
	for(z in 1:nbatch){
		props[z]<-sum(grp[[z]])/length(grp[[z]])  
	}
	
	
	#use this proportion to determine the number of arrays of each group to use in each batch.
	#using samplesize, which is the size of the smallest batch, sot hat things are consistent across arrays
	ss1 <- round(samplesize*props,0) #this gives you the number you should chose for grp=1
	ss0 <- samplesize-ss1 #this gives you the number you should chose for grp=0

	#determine which arrays from the original batches are grp0 versus grp1
	index1 <-list()
	index0<-list()
	for(z in 1:nbatch){
		index1[[z]] <- which(grp[[z]]==1)
		index0[[z]] <- which(grp[[z]]==0)
	}
	
		
	## Pooled stuff
	pooled.prop <- sum(pooledgrp)/length(pooledgrp)
	pooled.index0 <- which(pooledgrp==0)
	pooled.index1 <- which(pooledgrp==1)
	ss1p <- round(pooled.prop*samplesize,0)
	ss0p <- samplesize-ss1p


	for(i in 1:nrep){
		print("iteration:")
		print(i)
		#Randomly sample arrays from the original batches, according to the sample sizes determined above
		sample1 <- list()
		sample0 <- list()
		for(z in 1:nbatch){
			sample1[[z]]<-sample(index1[[z]],ss1[z],replace=F)
			sample0[[z]]<-sample(index0[[z]],ss0[z],replace=F)
		}
		
				
		#Now need to divide these arrays into build versus test sets.
		#Find length of samples
		lens1 <- lapply(sample1,length)
		lens0 <- lapply(sample0,length)
		vec0 <- list()
		vec1 <- list()
		
				
		#Divide in half into build and test sets - 0=build,1=test
		for(z in 1:nbatch){
			vec0[[z]]<-c(rep(0,lens0[[z]]/2),rep(1,lens0[[z]]/2))
			vec1[[z]]<-c(rep(0,lens1[[z]]/2),rep(1,lens1[[z]]/2))
		}	
		
		
		#Since the build/test group determination must be even (above), need to account for the fact that
		#the determined sample sizes above might be odd.
		#I randomly add a 0 or 1 to the end of vec (thus either making the build or the test group slightly larger.
		#this way I'm not systematically biasing things
		for(z in 1:nbatch){
			x0 <- lapply(vec0,length)
			x1 <- lapply(vec1,length)
			if(x0[[z]]!=lens0[[z]]){
				vec0[[z]][lens0[[z]]] <-  rbinom(1,1,.5)
			}
			if(x1[[z]]!=lens1[[z]]){
				vec1[[z]][lens1[[z]]] <-  rbinom(1,1,.5)
			}
		}
		
				
		build <- list()
		build.grp <- list()
		test <- list()
		test.grp <- list()

		for(q in 1:nbatch){
			index.build.0 <- sample0[[q]][vec0[[q]]==0]
			index.build.1 <- sample1[[q]][vec1[[q]]==0]
			index.test.0  <- sample0[[q]][vec0[[q]]==1]
			index.test.1  <- sample1[[q]][vec1[[q]]==1]
				
			build[[q]] <- c(index.build.0,index.build.1)
			test[[q]]  <- c(index.test.0,index.test.1)
			build.grp[[q]] <- c(grp[[q]][index.build.0],grp[[q]][index.build.1])
			test.grp[[q]]  <- c(grp[[q]][index.test.0],grp[[q]][index.test.1])
		}

		## Same for pooled
	
		sample.pooled.0.ss <- sample(pooled.index0,ss0p)
		sample.pooled.1.ss <- sample(pooled.index1,ss1p)

		lens1 <- length(sample.pooled.1.ss)
		lens0 <- length(sample.pooled.0.ss)

		#Divide in half into build and test sets - 0=build,1=test
		vec0<-c(rep(0,lens0/2),rep(1,lens0/2))
		vec1<-c(rep(0,lens1/2),rep(1,lens1/2))
			

		#Since the build/test group determination must be even (above), 
		#need to account for the fact that
		#the determined sample sizes above might be odd.
		#I randomly add a 0 or 1 to the end of vec (thus either making the 
		#build or the test group slightly larger).
		#This way I'm not systematically biasing things
		x0 <- length(vec0)
		x1 <- length(vec1)
		if(x0!=lens0){
			vec0[lens0] <-  rbinom(1,1,.5)
		}
		if(x1!=lens1){
			vec1[lens1] <-  rbinom(1,1,.5)
		}

		index.build.0 <- sample.pooled.0.ss[vec0==0]
		index.build.1 <- sample.pooled.1.ss[vec1==0]
		index.test.0  <- sample.pooled.0.ss[vec0==1]
		index.test.1  <- sample.pooled.1.ss[vec1==1]

		pooled.build <- c(index.build.0,index.build.1)
		pooled.test  <- c(index.test.0,index.test.1)
		pooled.build.grp <- c(pooledgrp[index.build.0],pooledgrp[index.build.1])
		pooled.test.grp  <- c(pooledgrp[index.test.0],pooledgrp[index.test.1])
			
		############## Build, test #################
		# from the steps above, I have the following important things:
		#	build, test - index for arrays used in each batch for building models
		#	build.grp, test.grp - groups for build, test sets
		#	pooled.build, pooled.test, pooled.build.grp, pooled.test.grp - obvious
		#############################################

		### PREPROCESSING! ###
		#don't need to do frma since it's single array#

		# RMA #
		rmabA<-exprs(rma(celA[,build[[1]]]))
		rmabB<-exprs(rma(celB[,build[[2]]]))
		rmab<-list(rmabA,rmabB)

		rmatA<-exprs(rma(celA[,test[[1]]]))
		rmatB<-exprs(rma(celB[,test[[2]]]))
		rmat<-list(rmatA,rmatB)

		rmabp<-exprs(rma(celp[,pooled.build]))
		rmatp<-exprs(rma(celp[,pooled.test]))

	
		# fRMA #
		frmabA<-frmaA[,build[[1]]]
		frmabB<-frmaB[,build[[2]]]
		frmab<-list(frmabA,frmabB)
	
		frmatA<-frmaA[,test[[1]]]
		frmatB<-frmaB[,test[[2]]]
		frmat<-list(frmatA,frmatB)

		frmabp<-frmap[,pooled.build]
		frmatp<-frmap[,pooled.test]
	
	
		######### LASSO ##########
		
		
		### temporary to speed up future work!! ##
		#globalwd<-"/home/bst/student/hiparker/PracticalBatch"
		#setwd(globalwd)
		#save.image("temp_for_lasso.RData")
		#load("temp_for_lasso.RData")
		#library(affy)
		#library(tspair)
		#library(pamr)
		#library(lars)
		####################################
		
		print("lasso.rma")
		lasso.rma <- list()
		pooled.lasso.rma <- c()
		sval <- c()
		
		
		for(s in 1:nbatch){
			lasso.rma[[s]] <- lars(t(rmab[[s]]), build.grp[[s]], use.Gram=FALSE)
			lasso_cv <- cv.lars(t(rmab[[s]]), build.grp[[s]], use.Gram=FALSE)
			sval[s] <- lasso_cv$index[which.min(lasso_cv$cv)]
		}
		pooled.lasso.rma <- lars(t(rmabp), pooled.build.grp, use.Gram=FALSE)
		lasso_cv <- cv.lars(t(rmabp), pooled.build.grp, use.Gram=FALSE)
		svalp <- lasso_cv$index[which.min(lasso_cv$cv)]

		
		
		mat <- matrix(nrow=nbatch,ncol=nbatch)
              ### Row = build batch, column = test batch ###
		for(j in 1:nbatch){
			for(k in 1:nbatch){
				x <- predict.lars(lasso.rma[[j]], t(rmat[[k]]),s=sval[j],mode="fraction")
				out <- rep(0,length(x$fit))
				out[x$fit > 0.5]<-1
				mat[j,k] <- sum(out==test.grp[[k]]) /
                            length(test.grp[[k]])
			}
		}
		x <- predict.lars(pooled.lasso.rma, t(rmatp),s=svalp,mode="fraction")
		out <- rep(0,length(x$fit))
		out[x$fit > 0.5]<-1
		pooled.rma.lasso.result[i] <- sum(out==pooled.test.grp)/length(pooled.test.grp)
		rma.lasso.result[[i]] <- mat

			
		print("lasso.frma")
		lasso.frma <- list()
		pooled.lasso.frma <- c()
		sval <- c()
		
		
		for(s in 1:nbatch){
			lasso.frma[[s]] <- lars(t(frmab[[s]]), build.grp[[s]], use.Gram=FALSE)
			lasso_cv <- cv.lars(t(frmab[[s]]), build.grp[[s]], use.Gram=FALSE)
			sval[s] <- lasso_cv$index[which.min(lasso_cv$cv)]
		}
		pooled.lasso.frma <- lars(t(frmabp), pooled.build.grp, use.Gram=FALSE)
		lasso_cv <- cv.lars(t(frmabp), pooled.build.grp, use.Gram=FALSE)
		svalp <- lasso_cv$index[which.min(lasso_cv$cv)]

		
		
		mat <- matrix(nrow=nbatch,ncol=nbatch)
              ### Row = build batch, column = test batch ###
		for(j in 1:nbatch){
			for(k in 1:nbatch){
				x <- predict.lars(lasso.frma[[j]], t(frmat[[k]]),s=sval[j],mode="fraction")
				out <- rep(0,length(x$fit))
				out[x$fit > 0.5]<-1
				mat[j,k] <- sum(out==test.grp[[k]]) /
                            length(test.grp[[k]])
			}
		}
		x <- predict.lars(pooled.lasso.frma, t(frmatp),s=svalp,mode="fraction")
		out <- rep(0,length(x$fit))
		out[x$fit > 0.5]<-1
		pooled.frma.lasso.result[i] <- sum(out==pooled.test.grp)/length(pooled.test.grp)
		frma.lasso.result[[i]] <- mat

		######### TSP #################
		## rma ##
            
		print("tsp.rma")
		tsp.rma <- list()
		pooled.tsp.rma <- c()
		for(s in 1:nbatch){
			tsp.rma[[s]] <- tspcalc(rmab[[s]], build.grp[[s]])
		}
		pooled.tsp.rma <- tspcalc(rmabp,pooled.build.grp)

		mat <- matrix(nrow=nbatch,ncol=nbatch)
              ### Row = build batch, column = test batch ###
		for(j in 1:nbatch){
			for(k in 1:nbatch){
				x <- predict(tsp.rma[[j]], rmat[[k]])
				mat[j,k] <- sum(x==test.grp[[k]]) /
                            length(test.grp[[k]])
			}
		}
		x <- predict(pooled.tsp.rma, rmatp)
		pooled.rma.tsp.result[i] <- sum(x==pooled.test.grp)/length(pooled.test.grp)
		rma.tsp.result[[i]] <- mat


		## frma ##
		print("tsp.frma")
		tsp.frma <- list()  
		pooled.tsp.frma <- c()
		for(s in 1:nbatch){
			tsp.frma[[s]] <- tspcalc(frmab[[s]],build.grp[[s]])
		}
		pooled.tsp.frma <- tspcalc(frmabp,pooled.build.grp)
       
		mat <- matrix(nrow=nbatch,ncol=nbatch)
			### Row = build batch, column = test batch
		for(j in 1:nbatch){
			for(k in 1:nbatch){
				x <- predict(tsp.frma[[j]], frmat[[k]])
				mat[j,k] <- sum(x==test.grp[[k]]) /
                            length(test.grp[[k]])
			}
		}
		x <- predict(pooled.tsp.frma, frmatp)
		pooled.frma.tsp.result[i] <- sum(x==pooled.test.grp)/length(pooled.test.grp)
		frma.tsp.result[[i]] <- mat

				 
	############## PAM ##################
    
		## rma ##
		print("pam.rma")

		pam.rma <- list()
		pooled.pam.rma <- c()
		for(s in 1:nbatch){
			pam.rma[[s]] <- pamr.train(list(
										x=rmab[[s]],
										y=build.grp[[s]]))
		}
		pooled.pam.rma <- pamr.train(list(
										x=rmabp,
										y=pooled.build.grp))

		thresh <- list()
		num.gene <- c()
		for(w in 1:nbatch){
			rev.fit <- rev(pam.rma[[w]]$error)              
			thresh[[w]]<- pam.rma[[w]]$threshold[length(rev.fit) -
                                             which.min(rev.fit)+1]
			num.gene[w]<-pam.rma[[w]]$nonzero[length(rev.fit) -
											which.min(rev.fit)+1]
		}
		rma.pam.numgenes[[i]] <- num.gene
            
		# pooled #
		rev.fit <- rev(pooled.pam.rma$error)              
		pooled.thresh <- pooled.pam.rma$threshold[length(rev.fit) -
                                              which.min(rev.fit)+1]
		pooled.rma.pam.numgenes[i] <- pooled.pam.rma$nonzero[length(rev.fit) -
                                                 which.min(rev.fit)+1]
            

		mat <- matrix(nrow=nbatch,ncol=nbatch)
                ### Row = build batch, column = test batch
		for(j in 1:nbatch){
			for(k in 1:nbatch){
				x <- pamr.predict(pam.rma[[j]],rmat[[k]],thresh[[j]])
				mat[j,k] <- sum(x == test.grp[[k]]) /
							length(test.grp[[k]])
			}
		}
		x <- pamr.predict(pooled.pam.rma,rmatp,pooled.thresh)
		pooled.rma.pam.result[i] <- sum(x == pooled.test.grp) / 
									length(pooled.test.grp)
		rma.pam.result[[i]] <- mat


		## frma ##

		print("pam.frma")

		pam.frma <- list()
		pooled.pam.frma <- c()
		for(s in 1:nbatch){
			pam.frma[[s]] <- pamr.train(list(
								x=frmab[[s]],
								y=build.grp[[s]]))
		}
		pooled.pam.frma <- pamr.train(list(
									x=frmabp,
									y=pooled.build.grp))

		thresh <- list()
		num.gene <- c()
		for(w in 1:nbatch){
			rev.fit <- rev(pam.frma[[w]]$error)              
			thresh[[w]]<- pam.frma[[w]]$threshold[length(rev.fit) -
											which.min(rev.fit)+1]
			num.gene[w]<-pam.frma[[w]]$nonzero[length(rev.fit) -
                                           which.min(rev.fit)+1]
		}
		# pooled #
		rev.fit <- rev(pooled.pam.frma$error)              
		pooled.thresh <- pooled.pam.frma$threshold[length(rev.fit) -
                                               which.min(rev.fit)+1]
		pooled.frma.pam.numgenes[i] <- pooled.pam.frma$nonzero[length(rev.fit) -
                                               which.min(rev.fit)+1]
            
		frma.pam.numgenes[[i]] <- num.gene

		mat <- matrix(nrow=nbatch,ncol=nbatch)
                ### Row = build batch, column = test batch
		for(j in 1:nbatch){
			for(k in 1:nbatch){
				x <- pamr.predict(pam.frma[[j]],
									frmat[[k]],
									thresh[[j]])
				mat[j,k] <- sum(x == test.grp[[k]]) /
							length(test.grp[[k]])
			}
		}
		x <- pamr.predict(pooled.pam.frma, frmatp, pooled.thresh)
		pooled.frma.pam.result[i] <- sum(x == pooled.test.grp) / 
									 length(pooled.test.grp)
		frma.pam.result[[i]] <- mat

    
		save(list=c("rma.tsp.result",
					"frma.tsp.result",
					"rma.lasso.result",
					"frma.lasso.result",
					"rma.pam.result",
					"rma.pam.numgenes",
					"frma.pam.result",
					"frma.pam.numgenes",  
                
					"pooled.rma.tsp.result",
					"pooled.rma.pam.result",
					"pooled.rma.lasso.result",
					"pooled.rma.pam.numgenes",
    
					"pooled.frma.tsp.result",
					"pooled.frma.pam.result",
					"pooled.frma.lasso.result",
					"pooled.frma.pam.numgenes",
                
					"nbatch",
					"pooledgrp"
					),
			file=paste(filename,".RData",sep="")
		)
	}
}