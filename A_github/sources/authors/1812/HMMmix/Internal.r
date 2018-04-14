  ## __________________________________________________________
  ##
  ## .ForwardR
  ##
  ## __________________________________________________________
  ##



  .ForwardR = function(Phi.vect,init,Mat.trans.norm.vect)
  {
  K 	= length(init)
  nbInd = length(Phi.vect)/K

  .C("Forward",as.double(Phi.vect),as.double(init),as.double(Mat.trans.norm.vect),as.integer(nbInd),as.integer(K),F=double(nbInd*K),Lambda=double(nbInd), PACKAGE="HMMmix")
  }


  ## __________________________________________________________
  ##
  ## .BackwardR
  ##
  ## __________________________________________________________
  ##

  .BackwardR = function(F,Mat.transition,K)
  {
  nbInd    = length(F)/K
  .C("Backward", as.double(F),as.double(Mat.transition),as.integer(nbInd),as.integer(K),tau=double(nbInd*K),G=double(nbInd*K), PACKAGE="HMMmix")
  }



  ## __________________________________________________________
  ##
  ## .InitialisationHMM
  ##
  ##
  ## __________________________________________________________
  ##


  .InitialisationHMM <- function(Tau)
  {

  nbInd    = dim(Tau)[1]
  K = dim(Tau)[2]

  ## Trasition Matrix
  Tau.tmp1 = Tau[-nbInd,]
  Tau.tmp2 = Tau[-1,]
  Mat.transition.tmp = t(Tau.tmp1)%*%Tau.tmp2
  Mat.transition = Mat.transition.tmp/rowSums(Mat.transition.tmp)

  ## Initial distribution
  val.propre = round(eigen(t(Mat.transition))$values,3)
  pos        = which(val.propre == 1.000)
  init       = eigen(t(Mat.transition))$vectors[,pos]
  init = init / sum(init)
  init = as.numeric(init)

  return(list(init=init,Mat.transition=Mat.transition))

  }


  ## __________________________________________________________
  ##
  ## .MatToVect
  ##
  ## __________________________________________________________
  ##


  .MatToVect <- function(Mat)
  {
  Mat	= as.matrix(Mat)
  nrow 	= dim(Mat)[1]
  ncol 	= dim(Mat)[2]
  vect 	= Mat[,1]

  if(ncol>=2)
  {
    for( i in 2:ncol)
    {
    vect = c(vect,Mat[,i])
    }
  }
  return(vect)

  }


  ##########################################################################
  ##
  ## .NonEmpty
  ##
  ##
  ##########################################################################


  .NonEmpty <-function(Tau)
  {
  K = dim(Tau)[2]

  for(k in 1:K)
  {
    Tau[,k] = pmax(Tau[,k],1e-10)
  }
  return(Tau)
  }






  ##########################################################################
  ##
  ## .converged
  ##
  ##########################################################################




  .converged  <- function(OldParameters,NewParameters,Threshold=1e-6)
  {

  Converged = FALSE

  if(max(abs(OldParameters-NewParameters)) <= Threshold)
  {
  Converged = TRUE
  }

  return(Converged)

  }




  ## __________________________________________________________
  ##
  ## .MatToVect.row
  ##
  ## __________________________________________________________
  ##


  .MatToVect.row <- function(Mat)
  {
  Mat=as.matrix(Mat)

  nrow = dim(Mat)[1]
  ncol = dim(Mat)[2]
  vect = Mat[1,]

  if(nrow>=2)
  {
    for( i in 2:nrow)
    {
    vect = c(vect,Mat[i,])
    }
  }
  return(vect)

  }


  ## __________________________________________________________
  ##
  ## .InitialKmeans
  ##
  ## __________________________________________________________
  ##



  .InitialKmeans<- function(data,K)
  {

  rk	= kmeans(data,K,nstart=25, iter.max = 100)
  d	= dim(data)[2]
  Tau 	= matrix(0,ncol=K,nrow=dim(data)[1])

    if(d>1)
    {
      for(k in 1:K)
      {
      Tau[,k] = dmnorm(data,rk$centers[k,],cov(data[rk$cluster==k,]))
      }
    }

    if(d==1)
    {
      for(k in 1:K)
      {
      Tau[,k] = dnorm(data,rk$centers[k],sd=sd(data[rk$cluster==k]))
      }
    }
  Tau=Tau/rowSums(Tau)
  return(Tau)

  }









  ## __________________________________________________________
  ##
  ## .Reestimate
  ##
  ## __________________________________________________________
  ##


  .Reestimate <- function (data, TauS, MatS, q, PhiS, PhiZ, MatInd, mean, sd,reestimateEM = 10)
  {
  n = dim(TauS)[1]
  K = dim(mean)[2]
  B = dim(TauS)[2]
  d = dim(mean)[1]
  TauZ = matrix(0, ncol = K, nrow = n)

  indmix = apply(MatInd, 1, .seekmix)
  mean = as.matrix(mean)
  for (i in 1:reestimateEM)
  {
  W = matrix(0, ncol = d, nrow = d)
    for (g in 1:B)
    {
    TauMix = PhiZ[, indmix[[g]]] * MatInd[g, indmix[[g]]]
    TauMix = as.matrix(TauMix)
    TauMix = apply(TauMix, 2, pmax, 1e-10)
    TauMix = TauMix/rowSums(as.matrix(TauMix))
    TauMix = as.matrix(TauMix)
    TauZ[, indmix[[g]]] = TauS[, g] * TauMix
    TauZ = apply(TauZ, 2, pmax, 1e-10)
    MatInd[g, indmix[[g]]] = colSums(as.matrix(TauZ[,indmix[[g]]]))/sum(TauZ[, indmix[[g]]])
    }
    mean[, 1:K] = apply(as.matrix(1:K), 1, FUN = function(k) colSums(TauZ[,k] * data)/colSums(TauZ)[k])
    if (d != 1)
    {
      for (k in 1:K)
      {
      tmp = t(apply(data, 1, .matW_c, k, mean))
      z = TauZ[, k] * tmp
      W = W + matrix(colSums(z), ncol = d, nrow = d)
      }
    sd = sqrt(sum(diag(W))/(d * n))
    }
    else sd = sqrt(sum(sapply(as.matrix(1:K), FUN = function(k) TauZ[,k] * (data - mean[, k])^2))/n)

  varcov = matrix(0, ncol = d, nrow = d)
  diag(varcov) = rep(sd^2, d)
  PhiZ[, 1:K] = apply(as.matrix(1:K), 1, FUN = function(k) dmnorm(data,mean = mean[, k], varcov))
  PhiS[, 1:B] = apply(as.matrix(1:B), 1, FUN = function(g) rowSums(as.matrix(PhiZ[,indmix[[g]]] * MatInd[g, indmix[[g]]])))
  PhiZ = apply(PhiZ, 2, pmax, 1e-50)
  PhiS = apply(PhiS, 2, pmax, 1e-50)
  EM = .EM.HMM(PhiS, q, MatS)
  TauS = EM$Tau
  F = EM$F
  G.tmp = EM$G
  TauS = TauS/rowSums(TauS)
  MatS = EM$Mat.count
  likeli = EM$loglik
  vp = eigen(MatS)
  val.propre = round(vp$values, 3)
  pos = which(val.propre == 1)
  q = vp$vectors[, pos]
  q = q/sum(q)
  q = as.numeric(q)
  }
  return(list(TauS = TauS, q = q, MatS = MatS, MatInd = MatInd,
        mean = mean, sd = sd, PhiZ = PhiZ, PhiS = PhiS,
        likeli = likeli,F=F,G=G.tmp))
  }


  ## __________________________________________________________
  ##
  ## .matW_c
  ##
  ## __________________________________________________________
  ##


  .matW_c = function(data,k,mu)
  {
   tmp = (data-mu[,k])%*%t(data-mu[,k])
   return(tmp)
  }


  ## __________________________________________________________
  ##
  ## .seekmix
  ##
  ## __________________________________________________________
  ##

  .seekmix<- function(vect)
  {
  ind = which(vect!=0)
  return(ind)
  }


  ## __________________________________________________________
  ##
  ## .Reduce
  ##
  ## __________________________________________________________
  ##


  .Reduce <- function(MatZ,Tau,n,i,j,MatInd)
  {
  Tau1		= Tau[1,]
  K		= dim(MatZ)[1]
  MatS		= matrix(0,K-1,K-1)
  Comptage	= MatZ *colSums(Tau)
  Buffer	= Comptage
  ind		= min(i,j)
  ind2		= max(i,j)
  Buffer	= Buffer[,-ind2]
  Buffer	= Buffer[-ind2,]
  MatS		= Buffer

  MatS[-c(ind),ind] 	= Comptage[-c(i,j),i]+Comptage[-c(i,j),j]
  MatS[ind,-c(ind)] 	= Comptage[i,-c(i,j)]+Comptage[j,-c(i,j)]
  MatS[ind,ind] 	= Comptage[i,j]+Comptage[j,i]+Comptage[i,i]+Comptage[j,j]
  MatS = MatS/rowSums(MatS)

  vp 		= eigen(MatS)
  val.propre	= round(vp$values,3)
  pos		= which(val.propre == 1.000)
  q		= vp$vectors[,pos]
  q		= q/sum(q)
  q		= as.numeric(q)
  prop 		= c(sum(Comptage[,i])+Tau1[i],sum(Comptage[,j])+Tau1[j])
  prop 		= prop/sum(prop)

  MatInd[i,] 	= prop[1]*MatInd[i,]
  MatInd[j,] 	= prop[2]*MatInd[j,]
  MatInd[ind,] 	= MatInd[ind,] + MatInd[ind2,]
  MatInd 	= MatInd[-ind2,]

  return(list(MatS=MatS,prop=prop,q=q,i=i,j=j,MatInd=MatInd))

  }



  ## __________________________________________________________
  ##
  ## .pruning
  ##
  ## __________________________________________________________
  ##


  .pruning <- function(MatZ,initZ,Tau,Phi,F,G,MatInd,pruning.th)
  {
  K = dim(MatZ)[1]
  range.i=0
  for(i in 1:K)
  {
  range.i = c(range.i,rep(i,K))
  }
  range.j = rep(seq(1:K),K)
  range.i = range.i[-1]

  test = mapply(FUN=function(i,j) .KL.Criterion(MatZ,initZ,Tau,Phi,i,j,F,G,MatInd),range.i,range.j)
  test = matrix(test,K,K,byrow=TRUE)
  test = test+t(test)
  diag(test) = rep(.Machine$double.xmax,K)

  ## 1ere colonne correspond a l'ordre des composantes les plus proches de la composante 1.
  range 	= apply(test,1,order)
  couple.j.tmp 	= numeric()
  couple.i	= numeric()
  couple.j 	= numeric()

  p = 1
  for(i in 1:(K-1))
  {
    for(j in (i+1):K)
    {
    couple.i[p] 	= i
    couple.j[p] 	= j
    couple.j.tmp[p] 	= test[i,j]
    p = p+1
    }
  }

  l	 	= length(couple.j.tmp)
  sort.j 	= sort(couple.j.tmp)
  order.j 	= order(couple.j.tmp)
  stop 		= ceiling(pruning.th*l)
  keep 		= order.j[(l-stop+1):l]
  couple.i	= couple.i[keep]
  couple.j 	= couple.j[keep]
  couple 	= rbind(couple.i,couple.j)

  return(couple)

  }




  ## __________________________________________________________
  ##
  ## .ModifTransition
  ##
  ## __________________________________________________________
  ##



  .ModifTransition <- function(MatS,prop,i,j)
  {

  K = dim(MatS)[1]+1
  Omega = matrix(0,ncol=K,nrow=K)
  ind1 = min(i,j)
  ind2 = max(i,j)

  if(ind2!=K)
  {
  Omega[1:(ind2-1),1:(ind2-1)] 	= MatS[1:(ind2-1),1:(ind2-1)]
  Omega[(ind2+1):K,1:(ind2-1)] 	= MatS[(ind2):(K-1),1:(ind2-1)]
  Omega[1:(ind2-1),(ind2+1):K] 	= MatS[1:(ind2-1),(ind2):(K-1)]
  Omega[(ind2+1):K,(ind2+1):K] 	= MatS[(ind2):(K-1),(ind2):(K-1)]
  Omega[,ind2] 			= prop[which.max(c(i,j))]*Omega[,ind1]
  Omega[,ind1] 			= prop[which.min(c(i,j))]*Omega[,ind1]
  Omega[ind2,] 			= Omega[ind1,]
  }

  else if(ind2==K)
  {
  Omega[1:(K-1),1:(K-1)] = MatS[1:(K-1),1:(K-1)]
  Omega[,K] = prop[which.max(c(i,j))]*Omega[,ind1]
  Omega[,ind1] = prop[which.min(c(i,j))]*Omega[,ind1]
  Omega[K,] = Omega[ind1,]
  }

  vp 		= eigen(Omega)
  val.propre	= round(vp$values,3)
  pos		= which(val.propre == 1.000)
  initOmega	= vp$vectors[,pos]
  initOmega	= initOmega/sum(initOmega)
  initOmega	= as.numeric(initOmega)

  return(list(Omega=Omega,initOmega = initOmega))
  }


  ## __________________________________________________________
  ##
  ## .KL.Criterion
  ##
  ## __________________________________________________________
  ##



  .KL.Criterion <- function(MatZ,initZ,Tau,Phi,i,j,F,G,MatInd)
  {

  if(i<j)
  {
  n 	= dim(Tau)[1]
  rde 	= .Reduce(MatZ,Tau,n,i,j,MatInd)
  MatS 	= rde$MatS
  prop 	= rde$prop
  q 	= rde$q

  TauBis = Tau
  GBis  = G
  FBis = F
  TauBis[,i] 	= Tau[,i] + Tau[,j]
  FBis[,i] 	= F[,i] + F[,j]
  GBis[,i] 	= G[,i] + G[,j]
  Phi[,i] 	= prop[1]*Phi[,i] + prop[2]*Phi[,j]
  TauBis 	= TauBis[,-j]
  FBis 		= FBis[,-j]
  GBis 		= GBis[,-j]
  Phi 		= Phi[,-j]
  rapportTauG 	= TauBis[-1,]/GBis[-1,]
  rapportFTau 	= FBis[-n,]/TauBis[-n,]
  rapportFTau 	= as.matrix(apply(rapportFTau,2,pmax,1e-20))
  ComptageS 	= MatS *colSums(TauBis)
  LogMatS	= log(MatS)
  sumTauBisq 	= sum(TauBis[1,]*log(q))
  tMatS		= t(MatS)

  tmp1.1 = rapportTauG%*%t(MatS*LogMatS)
  tmp1 	 = -sum(FBis[-n,]*tmp1.1)
  tmp2.1 = (rapportTauG*log(rapportTauG))%*%tMatS
  tmp2   = -sum(FBis[-n,]*tmp2.1)
  tmp3.1 = FBis[-n,]*log(rapportFTau)
  tmp3.2 = rapportTauG%*%tMatS
  tmp3   = -sum(tmp3.1*tmp3.2)

  Entropy 	= tmp1 + tmp2 + tmp3 - sumTauBisq
  term1 	= sum(ComptageS * LogMatS)
  term2		=  sum(TauBis*log(Phi))

  crit = (sumTauBisq + term1 + term2 +Entropy)

  return(crit)
  }

  else return(0)

  }


  ## __________________________________________________________
  ##
  ## .Critere_likelihood
  ##
  ## __________________________________________________________
  ##


  .Critere_likelihood <-function(reduceHMM,PhiS,PhiZ)
  {

  MatS	= reduceHMM$MatS
  prop	= reduceHMM$prop
  q	= reduceHMM$q
  i	= reduceHMM$i
  j	= reduceHMM$j
  MatInd = reduceHMM$MatInd
  n	= dim(PhiS)[1]
  B 	= ncol(PhiS)
  ind 	= min(i,j)
  ind2 	= max(i,j)

  PhiS[,ind] 	= prop[1]*PhiS[,i] + prop[2]*PhiS[,j]
  PhiS 		= PhiS[,-ind2]
  EM		= .EM.HMM(PhiS,q,MatS)
  Tau		= EM$Tau
  Tau		= Tau/rowSums(Tau)
  Comptage      = EM$Mat.count*n
  loglik       	= EM$loglik
  Entropy	= EM$Entropy
  crit.vect	= numeric()

  critere 	= loglik- (1+ 2*B+B*(B-1))/2 * log(n) ##BIC
  crit.vect[1] 	= critere
  crit.vect[2] 	= loglik -Entropy - (1+ 2*B+B*(B-1))/2 * log(n) ##ICLS

  F = EM$F
  G = EM$G

  return(list(Tau=Tau,PhiS=PhiS,loglik=loglik,Entropy=Entropy,critere=critere,crit.vect=crit.vect,F=F,G=G))
  }



  ## __________________________________________________________
  ##
  ## .E.step.HMM
  ##
  ## __________________________________________________________
  ##


  .E.step.HMM <- function(Phi,init,Mat.transition)
  {

  nbClasse = dim(Phi)[2]
   nbInd    = dim(Phi)[1]

  ## Transform matrix to vectors
  Phi.vect            = .MatToVect(Phi)
  Mat.transition.vect = .MatToVect(Mat.transition)


  ## Forward loop
  resF = .ForwardR(Phi.vect,init,Mat.transition.vect)

  ## Backward loop
  resB = .BackwardR(resF$F,Mat.transition.vect,nbClasse)

  ## Turn vector to matrix
  Tau   = matrix(resB$tau,nrow=nbInd,ncol=nbClasse,byrow=FALSE)
  F     = matrix(c(resF$F),nrow=nbInd,ncol=nbClasse)
  G     = matrix(c(resB$G),nrow=nbInd,ncol=nbClasse)

  Lambda	= resF$Lambda
  loglik 	= -sum(log(Lambda))
  vp 		= eigen(Mat.transition)
  val.propre	= round(vp$values,3)
  pos		= which(val.propre == 1.000)
  q		= vp$vectors[,pos]
  q		= q/sum(q)
  q		= as.numeric(q)

  ## Entropy
  Mat.transition = as.matrix(apply(Mat.transition,1,pmax,0.000001))
  Mat.transition = as.matrix(apply(Mat.transition,1,pmin,1))
  Mat.transition = Mat.transition/rowSums(Mat.transition)

  G   = as.matrix(apply(G,2,pmax,0.00000000001))
  Tau = as.matrix(apply(Tau,2,pmax,0.00000000001))
  rapportTauG = Tau[-1,]/G[-1,]
  rapportFTau = F[-nbInd,]/Tau[-nbInd,]


  tmp1.1 = rapportTauG%*%t(Mat.transition*log(Mat.transition))
  tmp1   = -sum(F[-nbInd,]*tmp1.1)
  tmp2.1 = (rapportTauG*log(rapportTauG))%*%t(Mat.transition)
  tmp2   = -sum(F[-nbInd,]*tmp2.1)
  tmp3.1 = F[-nbInd,]*log(rapportFTau)
  tmp3.2 = rapportTauG%*%t(Mat.transition)
  tmp3   = -sum(tmp3.1*tmp3.2)

  Entropy = tmp1 + tmp2 + tmp3 - sum(Tau[1,]*log(q))

  return(list(F=F,G=G,Tau=Tau,Lambda=Lambda,loglik=loglik,Entropy=Entropy))
  }




  ## __________________________________________________________
  ##
  ## .M.step.HMM
  ##
  ## __________________________________________________________
  ##


  .M.step.HMM <- function(Tau,F,G,Mat.transition)
  {
  nbInd    = dim(Tau)[1]
  nbClasse = dim(Tau)[2]

  ## Tansition matrix
  Mat.count.tmp = Mat.transition * (t(F[-nbInd,])%*% (Tau[-1,] / G[-1,]))
  Mat.count = Mat.count.tmp/pmax(rowSums(Mat.count.tmp),0.0001)
  Mat.count = t(as.matrix(apply(Mat.count,1,pmax,0)))
  Mat.count = t(as.matrix(apply(Mat.count,1,pmin,1)))

  return(list(Mat.count=Mat.count))
  }





  ## __________________________________________________________
  ##
  ## .EM.HMM
  ##
  ## __________________________________________________________
  ##



  .EM.HMM <- function(Phi,init,Mat.transition)
  {

  ## E step
  Estep = .E.step.HMM(Phi,init,Mat.transition)

  ## Gathering of E step results
  Tau          	= Estep$Tau
  Lambda	= Estep$Lambda
  loglik	= Estep$loglik
  Entropy     	= Estep$Entropy

  Tau          = Tau/rowSums(Tau)
  F            = Estep$F
  G            = Estep$G

  ## M step
  Mstep = .M.step.HMM(Tau,F,G,Mat.transition)

  ## Gathering of M step uptadating
  Mat.count = Mstep$Mat.count

  return(list(Tau=Tau,F=F,G=G,Mat.count=Mat.count,Lambda=Lambda,loglik=loglik,Entropy=Entropy))
  }



  ## __________________________________________________________
  ##
  ## .Gather
  ##
  ## __________________________________________________________
  ##

  .Gather <- function(data,nbGrp,Tau,MatZ,Phi,mean,sd,pruning.th=1,reestimateEM=10,F,G)
  {
  Phi = apply(Phi,2,pmax,0.0000001)
  n = dim(Tau)[1]
  K = dim(Tau)[2]
  MatS = MatZ
  likeli.vect = 0

  ## Distribution stationnaire
  vp 		= eigen(MatS)
  val.propre	= round(vp$values,3)
  pos		= which(val.propre == 1.000)
  q		= vp$vectors[,pos]
  q		= q/sum(q)
  q		= as.numeric(q)
  MatInd 	= matrix(0,K,K)
  diag(MatInd) 	= rep(1,K)
  PhiZ = Phi
  PhiS = Phi
  Entropy = 0
  critere = 0
  class 	= matrix(nrow=n,ncol=K)
  TAU     = list()
  MAT     = list()
  crit.mat = matrix(nrow=K-1,ncol=2)
  likeli.vect = numeric()

  while(K!=nbGrp)
  {
  likeli = -.Machine$double.xmax

  if(pruning.th==1)
  {
  range.i=0
    for(i in 1:K)
    {
    range.i = c(range.i,rep(i,K))
    }
  range.j = rep(seq(1:K),K)
  range.i=range.i[-1]
  }

  if(pruning.th!=1)
  {
  couple = .pruning(MatS,q,Tau,PhiS,F,G,MatInd,pruning.th)
  range.i = couple[1,]
  range.j = couple[2,]
  }

  test.tmp = mapply(FUN=function(i,j) .calcul(MatS,Tau,n,i,j,PhiS,PhiZ,MatInd,q),range.i,range.j)
  test = matrix(-.Machine$double.xmax,K,K,byrow=TRUE)

  for(l in 1:length(range.i))
  {
  test[range.i[l],range.j[l]] = test.tmp[l]
  }

  index = which(test==max(test),arr.ind=TRUE)
  ind1  = index[1,1]
  ind2  = index[1,2]
  rHMM  = .Reduce(MatS,Tau,n,ind1,ind2,MatInd)
  cl    = .Critere_likelihood(rHMM,PhiS,PhiZ)

  PhiS    = cl$Phi
  Tau	  = cl$Tau
  F 	  = cl$F
  G 	  = cl$G
  MatS	  = rHMM$MatS
  q	  = rHMM$q
  prop	  = rHMM$prop
  MatInd  = rHMM$MatInd
  K	  = dim(Tau)[2]
  Entropy = c(Entropy,cl$Entropy)
  critere = c(critere,cl$critere)

  if(reestimateEM>=1)
  {
  reete = .Reestimate(data,Tau,MatS,q,PhiS,PhiZ,MatInd,mean,sd,reestimateEM=reestimateEM)
  Tau	= reete$TauS
  q	= reete$q
  MatS	= reete$MatS
  MatInd= reete$MatInd
  mean	= reete$mean
  sd	= reete$sd
  PhiZ	= reete$PhiZ
  PhiS	= reete$PhiS
  likeli= reete$likeli
  F 	= reete$F
  G 	= reete$G
  }
  likeli.vect[K] 	= likeli
  class[,K]	= apply(Tau,1,which.max)
  TAU[[K]]  = Tau
  MAT[[K]]  = MatS
  crit.mat[K,] 	= cl$crit.vect
  }

  return(list(TAU=TAU,MAT=MAT,likeli.vect=likeli.vect,Phi=PhiS,q=q,Entropy=Entropy[-1],critere=critere[-1],class=class,crit.mat=crit.mat))
  }



  ## __________________________________________________________
  ##
  ## .calcul
  ##
  ## __________________________________________________________
  ##


  .calcul <- function(MatS,Tau,n,i,j,PhiS,PhiZ,MatInd,q)
  {

  if(i<j)
  {
  rHMM = .Reduce(MatS,Tau,n,i,j,MatInd)
  cl = .Critere_likelihood(rHMM,PhiS,PhiZ) 
  cl2 = cl$critere - cl$Entropy #ICLS
  return(cl$critere) #BIC
  #return(cl2)
  }

  else return(-.Machine$double.xmax)
  }


  ## __________________________________________________________
  ##
  ## .EMalgo
  ##
  ## __________________________________________________________
  ##


  .EMalgo <- function(data,K,IterationMax=1000,Threshold=1e-3)
  {

  ## Check
  stopifnot(K>=1)
  stopifnot(IterationMax>=1)

  ## Declaration and initialisation of local variables
  data  = as.matrix(data)
  n	= dim(data)[1]
  d	= dim(data)[2]
  Phi	= matrix(0,ncol=K,nrow=n)

  Tau 		= .InitialKmeans(data,K)
  Init.tau	= .InitialisationHMM(Tau)
  init	= Init.tau$init
  Mat.transition 	= Init.tau$Mat.transition
  mean        = matrix(0, ncol = K, nrow = d)
  OldParameters = rep(0,K*d+1)

  ######################## EM-algorithm ########################
  for(i in 1:IterationMax)
  {
  cat("\r","Number of iteration :",i)
  flush.console()
  W = matrix(0, ncol = d, nrow = d)
  nk = colSums(Tau)
  mean[, 1:K] = apply(as.matrix(1:K), 1, FUN = function(k) colSums(Tau[,k] * data)/nk[k])
  if (d != 1)
  {
    for (k in 1:K)
    {
    tmp = t(apply(data, 1, .matW_c, k, mean))
    z = Tau[,k]*tmp
    W = W + matrix(colSums(z), ncol = d, nrow = d)
    }
  sd = sqrt(sum(diag(W))/(d * n))
  }
  else sd = sqrt(sum(sapply(as.matrix(1:K), FUN = function(k) Tau[,k] * (data - mean[,k])^2))/n)

  varcov       = matrix(0, ncol = d, nrow = d)
  diag(varcov) = rep(sd^2, d)
  NewParameters = c(as.vector(mean),sd)
  Phi[,1:K]    = apply(as.matrix(1:K), 1, FUN = function(k) dmnorm(data,mean=mean[,k],varcov))

  ## E step
  EM	= .EM.HMM(Phi,init,Mat.transition)
  Tau	= EM$Tau
  Tau	= Tau/rowSums(Tau)
  F	= EM$F
  G	= EM$G

  Entropy		= EM$Entropy
  Mat.transition 	= EM$Mat.count
  vp = eigen(Mat.transition)
  val.propre = round(vp$values, 3)
  pos = which(val.propre == 1)
  q = vp$vectors[, pos]
  q = q/sum(q)
  init = as.numeric(q)

  ## Loglikelihood
  loglik = EM$loglik

  ### Too small values of Tau are not allowed
  Tau = .NonEmpty(Tau)

  ### Testing the algorithm convergence
  if(i>=2)
  {
  Test = .converged(OldParameters,NewParameters,Threshold=Threshold)
    if(Test)
    {
    break()
    }
  }

  ## Keep the old parameters
  OldParameters = NewParameters
  }

  parameters 	= list(mean = mean,varcov=varcov)
  transition 	= list(Mat.transition=Mat.transition)

  return(list(Tau=Tau,parameters=parameters,transition=transition,loglik=loglik,Phi=Phi,Test=Test,Entropy=Entropy,F=F,G=G))

  }

















