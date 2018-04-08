#' Identify the suitable number of components k
#' @description This function is used to find the suitable number of components k.
#' @param data - data in matrix formation with n rows and p columns
#' @param d - number of categories for each variable
#' @param TT - number of iterations in Gibbs sampler, default value is 1000. T should be an even number for 'burn-in'.
#' @param alpha - hyperparameter that could be regarded as the pseudo-count of the number of samples in the new component
#' @return k_est - posterior estimation of k
#' @return k_track - track of k in the iteration process
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
#' @export

kIdentifier<-function(data, d, TT = 1000, alpha = 0.25){
  # dimensional parameters
  n<-nrow(data)    # number of observations
  p<-ncol(data)    # number of variables

  # missing data
  d<-d+1           # including missing category
  for(j in 1:p){
    data[is.na(data[,j]),j]<-d[j]
  }

  # Bayesian inference
  # initial z, k
  k<-n
  z<-c(1:n)

  # initial psi
  psi<-list()
  for(j in 1:p){
    psi[[j]]<-rdirichlet(k,rep(1,d[j]))
  }

  # gibbs sampler
  # parameter track
  k_track<-rep(n,TT)
  # iteration
  for(t in 1:TT){
    # update z
    for(i in 1:n){
      # component probability
      zz<-z[-i]
      f<-rep(0,k+1)
      for(h in 1:k){
        f[h]<-length(zz[zz==h])/(n+alpha-1)
        for(j in 1:p){
          f[h]<-f[h]*(psi[[j]][h,data[i,j]])
        }
      }

      # new component probability
      f[k+1]<-alpha/(n+alpha-1)
      for(j in 1:p){
        f[k+1]<-f[k+1]/gamma(d[j]+1)
      }

      f<-f/sum(f)
      z[i]<-which(rmultinom(1,1,f)==1)

      # add component
      if(z[i]==k+1){
        k<-k+1
        for(j in 1:p){
          beta<-rep(1,d[j])
          beta[data[i,j]]<-beta[data[i,j]]+1
          psi[[j]]<-rbind(psi[[j]],rdirichlet(1,beta))
        }
      }
    }

    # update k
    class<-unique(z)
    k<-length(class)

    # reorder class to avoid label-switching
    nn<-rep(0,k)
    for(h in 1:k){
      nn[h]<-length(z[z==class[h]])
    }

    o<-order(nn,decreasing = TRUE)
    nn<-nn[o]
    z_temp<-z
    for(h in 1:k){
      z[z_temp==class[o[h]]]<-h
    }

    # update psi
    for(h in 1:k){
      for(j in 1:p){
        subdata<-data[z==h,j]
        beta<-rep(1,d[j])
        for(c in 1:d[j]){
          beta[c]<-beta[c]+length(subdata[subdata==c])
        }
        psi[[j]][h,]<-rdirichlet(1,beta)
      }
    }

    # parameter track
    k_track[t]<-k
  }

  # parameter estimate
  k_est<-as.numeric(names(which.max(table(k_track[(TT/2+1):TT]))))

  # output
  return(list(k_track = k_track,k_est = k_est))
}
