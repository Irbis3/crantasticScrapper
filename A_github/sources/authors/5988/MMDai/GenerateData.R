#' Generate random dataset
#' @description This function is used to generate random datasets following mixture of product multinomial distribution
#' @param n - number of samples
#' @param p - number of variables
#' @param d - a vector which denotes the number of categories for each variable. It could be distinct among variables.
#' @param k - number of latent classes
#' @param theta - probability for latent class
#' @param psi - probability for specific category
#' @return data - generated random dataset, a matrix with n rows and p columns.
#' @importFrom DirichletReg rdirichlet
#' @examples
#' # dimension parameters
#' n<-200; p<-5; d<-rep(2,p);
#' # generate complete data
#' Complete<-GenerateData(n, p, d, k = 3)
#' @export

GenerateData<-function(n, p, d, k = 3, theta = rdirichlet(1,rep(10,k)), psi = InitialPsi(p,d,k)){
  # initial dataset
  data<-matrix(0,nrow = n,ncol = p)

  # generate complete data
  for(i in 1:n){
    z<-which(rmultinom(1,1,theta)==1)
    for(j in 1:p){
      data[i,j]<-which(rmultinom(1,1,psi[[j]][z,])==1)
    }
  }

  # output
  return(data)
}
