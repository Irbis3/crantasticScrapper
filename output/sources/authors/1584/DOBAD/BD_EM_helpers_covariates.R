## Adding capability to handle covariates to some of the code.
## Here are functions analogous to some of those in BD_EM_helpers.R

## Note: oldCoefs refer to previous coefficients for 'zz', called gamma in the writeup.
## length(oldCoefs) = 2 * length(zz).
## oldCoefs should be px2 matrix where zz is a vector of length p.
## First row of coefficients corresponds to lambda, second row to mu.



## ZZ.LL and ZZ.MM are the lambda and mu design matrices, respectively -- log
## lambda = ZZ.LL %*% gamma.LL, log mu = ZZ.MM %*% gamma.MM
E.step.SC.cov <- function(theData, ZZ.LL, ZZ.MM,
                          ##oldCoefs,
                          oldCoefs.LL, oldCoefs.MM,
                          beta.immig,  dr=0.001,
                          n.fft=1024, r=4, prec.tol=1e-12, prec.fail.stop){
    ##oldParams <- exp(ZZ %*% oldCoefs);
    ##pp <- length(oldCoefs)/2
    ##oldParams <- exp(cbind(ZZ.LL %*% oldCoefs[1:pp], ZZ.MM%*% oldCoefs[(pp+1):(2*pp)]))
    oldParams <- exp(cbind(ZZ.LL %*% oldCoefs.LL, ZZ.MM %*% oldCoefs.MM))
    ##mm <- length(oldParams)/2
    mm <- length(getBDMCsPOlist(theData));
    ## only way i know to join oldParams and BDMCs.PO is with a for-loop anyway
    res <- matrix(NA, nrow=mm, ncol=3)
    for (i in 1:mm){
        res[i,] <- E.step.SC.CTMC_PO_1(theData[[i]], oldParams[i,], beta.immig,
                                       dr, n.fft, r, prec.tol, prec.fail.stop)
    }
    res
}


## M step -- for "many" as opposed to Estep which is for "ctmc_po_1"
## [now maybe e step .sc .cov is for many ]


## EMsuffStats is mx3 matrix, T is mx1 matrix or vector length m, params
## coefGuess is mx2 matrix.
## ZZ is m x p matrix.
## First column of EMsuffStats is U=E(N+|Y), 2nd is D=E(N-|Y), 3rd is P=E(R|Y).
## NOTE: nlmiterlim, nlmgradtol, nlmstepmax have 100,1e-9,.5 as arbitrarily chosen defaults!!
M.step.SC.cov <- function(EMsuffStats, Ts, ZZ.LL, ZZ.MM,
                          coefGuess.LL, coefGuess.MM,
                          beta.immig,
                          nlmiterlim=100, nlmgradtol=1e-9, nlmstepmax=.5 ){
    coefGuess <- c(coefGuess.LL, coefGuess.MM)
    pp.LL <- length(coefGuess.LL)
    pp.MM <- length(coefGuess.MM)
    ## nlm argument coefs needs to be vector
    El.nlm <- function(coefs){ ##El = E[l_c | Y], arg for nlm
        ##bdps <- exp(ZZ %*% coefs); ## Birth-Death ParamS
        logLL <- ZZ.LL%*% coefs[1:pp.LL]
        logMM <- ZZ.MM%*% coefs[(pp.LL+1):(pp.LL+pp.MM)]
        LL <- exp(logLL)
        MM <- exp(logMM)
        res <- sum(-EMsuffStats[,3]*(LL+MM) - Ts*beta.immig*LL +
                   EMsuffStats[,1]*logLL + EMsuffStats[,2]*logMM); ## why is this needed; ## this is off by an additive constant
        piece1.LL <- (EMsuffStats[,3]+beta.immig*Ts)*LL
        piece1.MM <- EMsuffStats[,3]*MM
        ##Previously, analytically computing gradient/hessian slowed things down
        ##Not sure if it is more or less stable
        attr(res,"gradient") <- c(t(ZZ.LL) %*% (-piece1.LL + EMsuffStats[,1]),
                                  t(ZZ.MM) %*% (-piece1.MM + EMsuffStats[,2]))
        attr(res,"hessian") <- as.matrix(bdiag(-t(ZZ.LL) %*%
                                               diag(as.vector(piece1.LL), nrow=length(piece1.LL)) %*%
                                               ZZ.LL, ## require(Matrix)
                                               -t(ZZ.MM) %*%
                                               diag(as.vector(piece1.MM),nrow=length(piece1.MM)) %*%
                                               ZZ.MM)) ##nlm won't allow non-numeric mode
        res;
    }

    newCoefs <- optim(coefGuess, fn=El.nlm,
                      method="BFGS", control=list(fnscale=-1))$par
    list(coefs.LL=newCoefs[1:pp.LL], coefs.MM=newCoefs[(pp.LL+1):(pp.LL+pp.MM)])
}

## NOTE: nlmiterlim, nlmgradtol, nlmstepmax have 100,1e-9,.5 as arbitrarily chosen defaults!!
getNewParams.SC.cov <- function(BDMCs.PO, ZZ.LL,ZZ.MM,
                                ##oldCoefs,
                                oldCoefs.LL, oldCoefs.MM,
                                beta.immig,
                                nlmiterlim=100, nlmgradtol=1e-9, nlmstepmax=.5,
                                dr=0.001, n.fft=1024,
                                r=4, prec.tol=1e-12, prec.fail.stop=TRUE){
    getT <- function(bdpo){
        tt <- getTimes(bdpo);
        tail(tt,1)-tt[1];
    }
    Ts <- sapply(getBDMCsPOlist(BDMCs.PO), getT)  ## taking [[ ]] operator not sufficient
    EMsuffStats <- E.step.SC.cov(BDMCs.PO, ZZ.LL,ZZ.MM,
                                 oldCoefs.LL, oldCoefs.MM,
                                 beta.immig,
                                 dr, n.fft, r, prec.tol, prec.fail.stop);
    tmp <- M.step.SC.cov(EMsuffStats, Ts=Ts, ZZ.LL,ZZ.MM,
                         oldCoefs.LL, oldCoefs.MM,
                         beta.immig,
                         nlmiterlim=nlmiterlim,
                         nlmgradtol=nlmgradtol,
                         nlmstepmax=nlmstepmax)
    ##browser()
    tmp
}


## test getNewParams ...
## ".1sv" = 1 starting value
## coefs.LL, coefs.MM are length pp.LL, pp.MM vectors, resp
## tol is tolerance for convergence, measured in L2 distance between iterations.
## M is integer >=1.
EM.BD.SC.cov.1sv <- function(BDMCs.PO,
                             ZZ.LL,ZZ.MM,
                             ##init.params,
                             coefs.LL.init, coefs.MM.init,
                             tol=1e-4,M=30, beta.immig,
                             dr=1e-7, n.fft=1024, r=4,
                             prec.tol=1e-12, prec.fail.stop=TRUE,
                             nlmiterlim=100, nlmgradtol=1e-9,nlmstepmax=.5,
                             verbose=1, verbFile=NULL){
    if (!is.null(verbFile)) sink(verbFile)
    eps <- 1/0
    estimators.LL <- coefs.LL.init
    estimators.MM <- coefs.MM.init
    pp.LL <- length(coefs.LL.init)
    pp.MM <- length(coefs.MM.init)
    estimators.hist <- list(coefs.LL=matrix(NA,ncol=M+1, nrow=pp.LL),
                            coefs.MM=matrix(NA,ncol=M+1, nrow=pp.MM))
    estimators.hist$coefs.LL[,1] <- coefs.LL.init
    estimators.hist$coefs.MM[,1] <- coefs.MM.init

    i <- 1;
    ##while (i<=M && (eps[1]>tol)){
    while (i<=M && (eps>tol)){
        estimators <- getNewParams.SC.cov(BDMCs.PO,ZZ.LL,ZZ.MM,
                                          ##estimators,
                                          estimators.LL, estimators.MM,
                                          beta.immig,
                                          nlmiterlim=nlmiterlim,nlmgradtol=nlmgradtol,
                                          nlmstepmax=nlmstepmax,
                                          dr,n.fft,r,prec.tol,prec.fail.stop)
        estimators.hist$coefs.LL[,M+1] <- estimators.hist$coefs.LL[,i+1] <-
            estimators.LL <- estimators$coefs.LL
        estimators.hist$coefs.MM[,M+1] <- estimators.hist$coefs.MM[,i+1] <-
            estimators.MM <- estimators$coefs.MM
        ## estimators.hist[,,i+1] <- estimators;
        ## estimators.hist[,,M+1] <- estimators; #for ease of access if tolerance kicks in
        if (verbose>0){
            print( paste("The", i, "just finished and the new estimators are"));
            print(estimators$coefs.LL);
            print(estimators$coefs.MM)
        }
        eps <- sqrt(sum(c(estimators$coefs.LL - estimators.hist$coefs.LL[,i],
                          estimators$coefs.MM - estimators.hist$coefs.MM[,i])^2)); ##L2 distance
        i <- i+1;
    }
    if(!is.null(verbFile)) sink(NULL)
    estimators.hist
}




