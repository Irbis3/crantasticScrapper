  HMMmix<-function(	data,K=5, D=4, IterationMax=1000, Threshold=1e-3,pruning.th=1,reestimateEM=1)
  {
  
  ## Check
  stopifnot(D<=K)
  stopifnot(IterationMax>=1)

  ## Declaration and initialisation of local variables
  data		      = as.matrix(data)
  Converge    	= TRUE
  n	= dim(data)[1]
    
 	cat("\n","Inferring the parameters for the model with ",K," components...")
	cat("\n")
 	estimation = .EMalgo(data,K,IterationMax=IterationMax,Threshold=Threshold)

	
  loglik 	 = estimation$loglik
  Test 	         = estimation$Test
  F   	         = estimation$F
  G   	         = estimation$G
  Mat.transition = estimation$transition$Mat.transition
  Phi = estimation$Phi
  Tau = estimation$Tau
  Entropy = estimation$Entropy
  mean = estimation$parameters$mean
  sd   = sqrt(estimation$parameters$varcov[1,1])

  if(D!=K)
  {
  cat("\n","Combining the components... ")
  gather = .Gather(data,D,Tau,Mat.transition,Phi,mean,sd,pruning.th=pruning.th,reestimateEM=reestimateEM,F,G)
  MAT = gather$MAT
  TAU = gather$TAU
  class = gather$class
  class[,K]	= apply(Tau,1,which.max)
  TAU[[K]]  = Tau
  MAT[[K]]  = Mat.transition
  crit.mat = gather$crit.mat
  DstatesHMM = list(Tau=TAU[[D]],Mat.transition=MAT[[D]],class=class[,D])
  MaxICLstatesHMM = list(Tau=TAU[[which.max(crit.mat[,2])]],Mat.transition=MAT[[which.max(crit.mat[,2])]],class=class[,which.max(crit.mat[,2])])
  colnames(crit.mat) = c("BIC","ICLs")
  cat("\n")	
  loglik = gather$likeli.vect
  }
  res = list(DstatesHMM=DstatesHMM,MaxICLstatesHMM=MaxICLstatesHMM,loglik=loglik,estimation=estimation,crit.mat=crit.mat,data=data,class=class,TAU=TAU,MAT=MAT)

  attr(res,"class") <-"HMMmix"
  return(res)
  }
  



  
  
