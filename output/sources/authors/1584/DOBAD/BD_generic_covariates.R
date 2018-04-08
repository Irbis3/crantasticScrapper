##



BDloglikelihood.PO.cov <- function(partialData,
                                   coefs.LL, coefs.MM, beta.immig,
                                   object.LL=NULL, data.LL=NULL,
                                   object.MM=object.LL, data.MM=data.LL,
                                   ZZ.LL=NULL, ZZ.MM=ZZ.LL,
                                   delta=1e-4, r=4, n.fft=1024){ ##ctmcpo1 or ctmcpomany
  ##get ZZ design matrices
  if (!is.null(object.LL) && !is.null(data.LL)){
    ZZ.LL <- model.matrix(object.LL,data.LL)
    ZZ.MM <- model.matrix(object.MM,data.MM)
  } ## else use the passed-in ZZ.LL and ZZ.MM vals.
  ## compute birth,death,immig rates from coefficients&covariates
  ##pp.LL <- length(coefs.LL)
  ##pp.MM <- length(coefs.MM)
  ##LLs <- exp(ZZ.LL %*% theta[1:pp.LL])
  ##MMs <- exp(ZZ.MM %*% theta[(pp.LL+1):(pp.LL+pp.MM)])
  BDloglikelihood.PO.cov.helper(partialData,
                                coefs.LL, coefs.MM, beta.immig,
                                ZZ.LL, ZZ.MM,
                                n.fft)
}

## Reason for this function is that it's slightly more efficient if the
## loglikelihood is being computed repeatedly, as in getting the information
## numerically.
BDloglikelihood.PO.cov.helper <- function(partialData, coefs.LL, coefs.MM, beta.immig,
                                          ZZ.LL, ZZ.MM=ZZ.LL,
                                          n.fft=1024){ ##ctmcpo1 or ctmcpomany
  LLs <- exp(ZZ.LL %*% coefs.LL);
  MMs <- exp(ZZ.MM %*% coefs.MM);
  NNs <- beta.immig*LLs
  sum(mapply(BDloglikelihood.PO.CTMC_PO_1,
             partialDat=getBDMCsPOlist(partialData), L=LLs,m=MMs,nu=NNs,
             n.fft=n.fft,
             SIMPLIFY=TRUE))
}


getBDinform.cov.numeric <- function(partialData,
                                    coefs.LL, coefs.MM, beta.immig,
                                    object.LL=NULL, data.LL=NULL,
                                    object.MM=object.LL, data.MM=data.LL,
                                    ZZ.LL=NULL, ZZ.MM=ZZ.LL,
                                    delta=1e-4, r=4, n.fft=1024){
  ##get ZZ design matrices
  if (!is.null(object.LL) && !is.null(data.LL)){
    ZZ.LL <- model.matrix(object.LL,data.LL)
    ZZ.MM <- model.matrix(object.MM,data.MM)
  } ## else use the passed-in ZZ.LL and ZZ.MM vals.
  ## compute birth,death,immig rates from coefficients&covariates
  pp.LL <- length(coefs.LL)
  pp.MM <- length(coefs.MM)
  myLogLike <- function(theta){ ##ctmcpo1 or ctmcpomany
    BDloglikelihood.PO.cov.helper(partialData,
                                  coefs.LL=theta[1:pp.LL],
                                  coefs.MM=theta[(pp.LL+1):(pp.LL+pp.MM)],
                                  beta.immig, ZZ.LL, ZZ.MM, n.fft)
  }
  ##al the work is done by genD ...
  Ihat <- -hessian(myLogLike, x=c(coefs.LL,coefs.MM), method.args=list(d=delta,r=r))
  ##return(Ihat)
  DD <- t(genD(myLogLike, x=c(coefs.LL,coefs.MM),
               method.args=list(d=delta,r=r))$D[1,1:(pp.LL+pp.MM),drop=FALSE])
  list(Ihat=Ihat, DD2=DD %*% t(DD), DD=DD)
}
