
## Moment computations for computing the information, for restricted model with covariates

## Information is formed by computing Z' (stats1)(stats2)' Z for various
## values of Z and statsi.

##   ##all.cond.mean2.PO(data,LL,MM,NN,delta,n.fft,prec.tol,prec.fail.stop)
## }

## This computes the 'inner' m x m matrix (where m is # of individuals),
## denoted (stats1)(stats2)' above, formed by expectation of the outer
## product of two vectors of the 'statistics' N+,N-,R.  See the writeup.
## object.LL,data.LL, object.MM,data.MM, ZZ.LL,ZZ.MM have same documentation
## as in EM.BD.SC.cov.
## 'coefs' is px2 matrix
## Requires m >= 2, otherwise use getBDinform.PO.SC.
getBDinform.PO.SC.cov <- function(BDMCs.PO,
                                  coefs.LL, coefs.MM=coefs.LL, beta.immig,
                                  ##LL,MM,NN,
                                  object.LL=NULL, data.LL=NULL,
                                  object.MM=object.LL, data.MM=data.LL,
                                  ZZ.LL=NULL, ZZ.MM=NULL,
                                  delta=1e-4, n.fft=512, r=4,
                                  prec.tol=1e-12, prec.fail.stop=TRUE,
                                  verbose=0){
  ##get ZZ design matrices
  if (!is.null(object.LL) && !is.null(data.LL)){
    ZZ.LL <- model.matrix(object.LL,data.LL)
    ZZ.MM <- model.matrix(object.MM,data.MM)
  } ## else use the passed-in ZZ.LL and ZZ.MM vals.
  ## compute birth,death,immig rates from coefficients&covariates
  LLs <- exp(ZZ.LL %*% coefs.LL)
  MMs <- exp(ZZ.MM %*% coefs.MM)
  NNs <- beta.immig * LLs;
  if (verbose>=1) {print(paste("LLs,MMs,NNs are")); print(cbind(LLs,MMs,NNs))}
  TTs <- matrix(getTs(BDMCs.PO),ncol=1) ## New ; should return vector of Ts
  if (verbose>=1) print(paste("TTs is ", TTs))

  ## HERE - 'dobad:::'
  EEs <- t(mapply(all.cond.mean2.PO,
                  data=getBDMCsPOlist(BDMCs.PO), ## !!!! can't pass the CTMC_PO_many! need list!
                  lambda=LLs,mu=MMs,nu=NNs,
                  MoreArgs=list(delta=delta,n=n.fft,r=r,
                    prec.tol=prec.tol,prec.fail.stop=prec.fail.stop),
                  SIMPLIFY=TRUE)) ## EE == expectations

  if (verbose>=1) {print(paste("Expectations, EEs, are ")); print(EEs)}
  ## t(ZZ.LL) %*% LL.LL.inner (before adding the diagonal)  %*% ZZ.LL should be 0! (at MLE)
  innerpiece.LL <- (-(EEs[,9]+beta.immig*TTs)*LLs + EEs[,7])
  innerpiece.MM <- -(EEs[,9])*MMs + EEs[,8];
  LL.LL.inner <- LL.LL.inner0 <- innerpiece.LL %*% t(innerpiece.LL) ## "A"
  LL.MM.inner <- LL.MM.inner0 <- innerpiece.LL %*% t(innerpiece.MM) ## "B"
  MM.MM.inner <-   MM.MM.inner0 <- innerpiece.MM %*% t(innerpiece.MM) ## "C"
  ## ,drop=FALSE not needed here
  diag(LL.LL.inner) <- (EEs[,3] - 2*EEs[,9]*beta.immig*TTs + (TTs*beta.immig)^2) * LLs^2 -
    2*EEs[,5]*LLs - 2*TTs*beta.immig*LLs*EEs[,7] + EEs[,1];
  diag(LL.MM.inner) <- EEs[,3]*LLs*MMs + beta.immig*TTs*LLs*MMs*EEs[,9] -
    EEs[,6]*LLs - beta.immig*TTs*LLs*EEs[,8] -
      EEs[,5]*MMs + EEs[,4];
  diag(MM.MM.inner) <- EEs[,3]*MMs^2 - 2*MMs*EEs[,6] + EEs[,2]
  AA <- t(ZZ.LL) %*% LL.LL.inner %*% ZZ.LL;
  BB <- t(ZZ.LL) %*% LL.MM.inner %*% ZZ.MM;
  CC <- t(ZZ.MM) %*% MM.MM.inner %*% ZZ.MM;
  squaredgradient <- rbind(cbind(AA, BB), cbind(t(BB),CC))
  ## drop=TRUE, note: diag(matrix) _reads off the diagonal of the matrix_
  tmp <- as.vector((EEs[,9] + beta.immig*TTs)*LLs)
  jacobian.LL <- t(-ZZ.LL) %*% diag(tmp, nrow=length(tmp)) %*% ZZ.LL;
  tmp <- as.vector(EEs[,9]*MMs)
  jacobian.MM <- t(-ZZ.MM) %*% diag(tmp,nrow=length(tmp)) %*% ZZ.MM;
  jacobian <- Matrix::.bdiag(lst=list(jacobian.LL,jacobian.MM)) ## require(Matrix)
  ##browser()
  ## return(list(-jacobian, -squaredgradient, -jacobian-squaredgradient,
  ##             EEs=EEs,
  ##             AA=AA,BB=BB,CC=CC,
  ##             ZZ.LL=ZZ.LL, ZZ.MM=ZZ.MM,
  ##             LL.LL.inner0=              LL.LL.inner0,
  ##             LL.MM.inner0=LL.MM.inner0, MM.MM.inner0=MM.MM.inner0,
  ##             zero.LL=t(ZZ.LL) %*% innerpiece.LL, zero.MM = t(ZZ.MM) %*% innerpiece.MM,
  ##             LL.LL.inner=LL.LL.inner, LL.MM.inner=LL.MM.inner,MM.MM.inner=MM.MM.inner,
  ##             innerpiece.LL=innerpiece.LL, innerpiece.MM=innerpiece.MM))
  return(-jacobian-squaredgradient) # Plus 0 at the MLE!
}


########################################################################
#### Information via numeric/derivative methods ########################
########################################################################

getBDinform.PO.SC.cov.numeric <- function(BDMCs.PO,
                                coefs.LL, coefs.MM=coefs.LL,
                                beta.immig,
                                object.LL=NULL, data.LL=NULL,
                                object.MM=object.LL, data.MM=data.LL,
                                ZZ.LL=NULL, ZZ.MM=NULL,
                                delta=1e-4,
                                r=4,
                                n.fft=1024){
  if (!is.null(object.LL) && !is.null(data.LL)){
    ZZ.LL <- model.matrix(object.LL,data.LL)
    ZZ.MM <- model.matrix(object.MM,data.MM)
  } ## else use the passed-in ZZ.LL and ZZ.MM vals.
  pp.LL <- dim(ZZ.LL)[2];
  pp.MM <- dim(ZZ.MM)[2];



  myll<- function(theta){ ##ctmcpo1 or ctmcpomany
    BDloglikelihood.PO.cov.helper(BDMCs.PO,
                                  coefs.LL=theta[1:pp.LL],
                                  coefs.MM=theta[(pp.LL+1):(pp.LL+pp.MM)],
                                  beta.immig=beta.immig,
                                  ZZ.LL, ZZ.MM,
                                  n.fft=n.fft);
  }
  ##Ihat <- -matrix(hessian(myll,x=c(Lhat,Mhat), method.args=list(d=delta, r=r)))
  Ihat <- -hessian(myll,x=c(coefs.LL,coefs.MM), method.args=list(d=delta, r=r))
  return(Ihat)
}

########################################################################
#### END Information via numeric/derivative methods ####################
########################################################################
