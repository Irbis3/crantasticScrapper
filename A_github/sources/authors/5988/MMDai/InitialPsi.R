#' initial psi
#' @description This function creates a psi list in that each component has equal weight
#' @param p - number of variables
#' @param d - a vector which denotes the number of categories for each variable. It could be distinct among variables.
#' @param k - number of components
#' @return psi - a list in that each component has equal weight
#' @importFrom DirichletReg rdirichlet
#' @export

InitialPsi<-function(p, d, k){
  # psi list
  psi<-list()

  # each component has equal weight
  for(j in 1:p){
    psi[[j]]<-matrix(rdirichlet(k,rep(0.5,d[j])),nrow = k,ncol = d[j])
  }

  # return
  return(psi)
}
