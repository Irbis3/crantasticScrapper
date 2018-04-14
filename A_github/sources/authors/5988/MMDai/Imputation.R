#' Imputation
#' @description This function is used to perform multiple imputation for missing data given the joint distribution.
#' @param data - incomplete dataset
#' @param theta - vector of probability for each component
#' @param psi - specific probability for each variable in each component
#' @return ImputedData - dataset has been imputated.
#' @import stats
#' @export


# imputation
Imputation<-function(data, theta, psi){
  # dimensional parameters
  p<-ncol(data)    # number of variables
  n<-nrow(data)    # number of observations
  k<-length(theta) # number of components

  d<-rep(0,p)   # category for each variable
  for(j in 1:p){
    d[j]<-ncol(psi[[j]])
  }

  # multiple imputation
  ImputedData<-data

  # imputation
  for(i in 1:n){
    if(any(is.na(data[i,]))){
      miss<-which(is.na(data[i,]))
      obs<-which(!is.na(data[i,]))

      # observation likelihood
      ObsProb<-theta
      for(h in 1:k){
        for(jj in obs){
          ObsProb[h]<-ObsProb[h]*psi[[jj]][h,data[i,jj]]
        }
      }

      # missing conditional probability
      for(j in miss){
        CondProb<-rep(0,d[j])
        for(c in 1:d[j]){
          for(h in 1:k){
            CondProb[c]<-CondProb[c]+ObsProb[h]*psi[[j]][h,c]
          }
        }

        ImputedData[i,j]<-which.max(CondProb)
      }
    }
  }

  # output
  return(ImputedData)
}
