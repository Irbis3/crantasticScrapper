#' Estimate theta and psi in multinomial mixture model
#' @description This function is used to estimate theta and psi in multinomial mixture model given the number of components k.
#' @param data - data in matrix formation with n rows and p columns
#' @param d - number of categories for each variable
#' @param k - number of components
#' @param TT - number of iterations in Gibbs sampler, default value is 1000. T should be an even number for 'burn-in'.
#' @return theta - vector of probability for each component
#' @return psi - specific probability for each variable in each component
#' @import stats
#' @importFrom DirichletReg rdirichlet
#' @examples
#' # dimension parameters
#' n<-200; p<-5; d<-rep(2,p);
#' # generate complete data
#' Complete<-GenerateData(n, p, d, k = 3)
#' # mask percentage of data at MCAR
#' Incomplete<-Complete
#' Incomplete[sample(1:n*p,0.2*n*p,replace = FALSE)]<-NA
#' # k identify
#' K<-kIdentifier(data = Incomplete, d, TT = 10)
#' Par<-ParEst(data = Incomplete, d, k = K$k_est, TT = 10)
#' @export

ParEst<-function(data, d, k, TT = 1000){
  # dimensional parameters
  p<-ncol(data)    # number of variables
  n<-nrow(data)    # number of observations

  # missing data
  d<-d+1           # including missing category
  for(j in 1:p){
    data[is.na(data[,j]),j]<-d[j]
  }

  # Bayesian inference
  # initial theta
  theta<-rdirichlet(1,rep(1,k))

  # initial psi
  psi<-list()
  for(j in 1:p){
    psi[[j]]<-rdirichlet(k,rep(1,d[j]))
  }

  # initial z
  z<-rep(0,n)
  for(i in 1:n){
    z[i]<-which(rmultinom(1,1,theta)==1)
  }

  # gibbs sampler
  # initial log posterior probability
  pp_max<-0
  for(i in 1:n){
    for(j in 1:p){
      pp_max<-pp_max+log(psi[[j]][z[i],data[i,j]])
    }
  }

  # initial estimator
  theta_est<-theta
  psi_est<-psi
  for(t in 1:TT){
    # update z
    z<-rep(0,n)   # latent class
    for(i in 1:n){
      f<-rep(0,k)
      for(h in 1:k){
        f[h]<-theta[h]
        for(j in 1:p){
          f[h]<-f[h]*(psi[[j]][h,data[i,j]])
        }
      }

      f<-f/sum(f)
      z[i]<-which(rmultinom(1,1,f)==1)
    }

    # reorder class to avoid label-switching
    nn<-rep(0,k)
    for(h in 1:k){
      nn[h]<-length(z[z==h])
    }

    o<-order(nn,decreasing = TRUE)
    nn<-nn[o]
    z_temp<-z
    for(h in 1:k){
      z[z_temp==o[h]]<-h
    }

    # update theta
    theta<-rdirichlet(1,nn+rep(1,k))

    # update psi
    for(j in 1:p){
      for(h in 1:k){
        subdata<-data[z==h,j]
        beta<-rep(1,d[j])
        for(c in 1:d[j]){
          beta[c]<-beta[c]+length(subdata[subdata==c])
        }
        psi[[j]][h,]<-rdirichlet(1,beta)
      }
    }

    # posterior probability
    pp<-0
    for(i in 1:n){
      for(j in 1:p){
        pp<-pp+log(psi[[j]][z[i],data[i,j]])
      }
    }

    # parameter estimation
    if(pp>pp_max){
      theta_est<-theta
      psi_est<-psi
      pp_max<-pp
    }
  }

  # renormalize
  for(j in 1:p){
    for(h in 1:k){
      psi_est[[j]][h,1:(d[j]-1)]<-psi_est[[j]][h,1:(d[j]-1)]/(1-psi_est[[j]][h,d[j]])
    }
    psi_est[[j]]<-psi_est[[j]][,1:(d[j]-1)]

    # avoid to reduce to vector
    if(k==1){
      psi_est[[j]]<-matrix(psi_est[[j]],nrow = 1)
    }
  }

  # output
  return(list(theta = theta_est, psi = psi_est))
}
