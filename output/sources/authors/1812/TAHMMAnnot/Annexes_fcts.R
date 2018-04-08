###Fonction qui calcule -(1/2)*(x-mu)*sigma^-1*t(x-mu)###
ProdMatGauss = function(A,B)
{
  Prod = -(1/2)* A %*% B %*% t(A);
  return(Prod); 
}


###Pour acceder à un élément d'une matrice dans une liste###
Acces = function(mat,i,j)
{
  x = mat[i,j]
  return(x)
}


###Fonction de calcul de tau###
# Astuce de calcul pour eviter que ca plante si on a des chiffres trop petits :
# on enleve la moyenne a tout le monde dans l'exponentielle

#tau = function(x,sigma,mu,Pi)
#{
#  inv.sigma<-lapply(sigma,solve)
#  det.sigma<-unlist(lapply(sigma,det)) #vecteur
#  coeff=1/((2*pi)*sqrt(det.sigma)); #vecteur

#  tmp = lapply(mu,"-",x) #liste mu-x
#  tmp = lapply(tmp,"-") #liste x-mu

#  P = mapply(ProdMatGauss,tmp,inv.sigma) #vecteur: -(1/2)*(x-mu)*sigma^-1*t(x-mu)
#  P = P-mean(P) #on enleve la moyenne pour pas que l'exp explose

#  t =  Pi*coeff*exp(P) / sum( Pi*coeff*exp(P)) 
#  return(t)
#}

###Fonction de calcul de tau plus rapide ###
tau = function(x,sigma,mu,Pi)
{

  Phi=matrix(0,nrow=dim(x)[1],ncol=4)
  Phi[,1:4] = sapply(1:4,FUN = function(k) Pi[k]*dmnorm(x,mean=as.vector(mu[[k]]),varcov=sigma[[k]]))
  
  t =  Phi/rowSums(Phi)
  return(t)
}

###Fonction de calcul de tau avec annot###
# Astuce de calcul pour eviter que ca plante si on a des chiffres trop petits :
# on enleve la moyenne a tout le monde dans l'exponentielle

tauannot = function(xbis,sigma,mu,Pi)
{
  inv.sigma<-lapply(sigma,solve)
  det.sigma<-unlist(lapply(sigma,det)) #vecteur
  coeff=1/((2*pi)*sqrt(det.sigma)); #vecteur

  tmp = lapply(mu,"-",c(xbis[1],xbis[2])) #liste mu-x
  tmp = lapply(tmp,"-") #liste x-mu

  P = mapply(ProdMatGauss,tmp,inv.sigma) #vecteur: -(1/2)*(x-mu)*sigma^-1*t(x-mu)
  P = P-mean(P) #on enleve la moyenne pour pas que l'exp explose

  t =  Pi[,xbis[3]]*coeff*exp(P) / sum( Pi[,xbis[3]]*coeff*exp(P)) 
  return(t)
}


###Fonction de calcul de D###
CalculDirect = function(W)
{
 
  w1 = unlist(lapply(W,Acces,i=1,j=1))
  w4 = unlist(lapply(W,Acces,i=2,j=2))
  w2 = unlist(lapply(W,Acces,i=1,j=2))

  t = sum(w1[1:2]) - sum(w4[1:2])
  v = sum(w2[1:2])
 
  b = t^2 + 4*(v^2) 
  if(t>0)
  {
    x = (1/2)*(1+(t/sqrt(b)))
  }  else {
    x = (1/2)*(1-(t/sqrt(b)))
  }
  d = sqrt(x)
  Dmat = matrix(c(d,sqrt(1-d^2),-sqrt(1-d^2),d),nrow=2,ncol=2)
  return(Dmat)
}


###Fonction de calcul de W###
matW = function(x,k,mu)
{
  q = t(x-mu[[k]])%*%(x-mu[[k]])
  return(q)
}


###Fait un produit de matrices###
ProdMat = function(A,B)
{
  Prod = t(A) %*% B %*% A;
  return(Prod); 
}


###Fait un produit de matrices pour sigma###
#on ne peut pas utiliser ProdMat car il ne comprend pas qd on passe t(D) en argument
ProdMat2 = function(A,B)
{
  Prod = A %*% B %*% t(A);
  return(Prod); 
}



ForwardAnnotrestart0R = function(Phi.vect,muHMM.vect,Mat.trans.norm.vect,annot,diffannot,n,K,p)
{
  .C("Forward_Annot_restart0",as.double(Phi.vect),as.double(muHMM.vect),as.double(Mat.trans.norm.vect),as.integer(annot),as.integer(diffannot),as.integer(n),as.integer(K),as.integer(p),F=double(n*K), PACKAGE="TAHMMAnnot")

}

BackwardAnnotrestart0R = function(muHMM.vect,F,Mat.trans.norm.vect,annot,diffannot,n,K,p)
{
  .C("Backward_Annot_restart0", as.double(muHMM.vect),as.double(F),as.double(Mat.trans.norm.vect),as.integer(annot),as.integer(diffannot),as.integer(n),as.integer(K),as.integer(p),tau=double(n*K),G=double(n*K), PACKAGE="TAHMMAnnot")
}

ForwardAnnotR = function(Phi.vect,muHMM.vect,Mat.trans.norm.vect,annot,n,K,p)
{
  .C("Forward_Annot",as.double(Phi.vect),as.double(muHMM.vect),as.double(Mat.trans.norm.vect),as.integer(annot),as.integer(n),as.integer(K),as.integer(p),F=double(n*K), PACKAGE="TAHMMAnnot")

}

BackwardAnnotR = function(F,Mat.trans.norm.vect,annot,n,K,p)
{
  .C("Backward_Annot", as.double(F),as.double(Mat.trans.norm.vect),as.integer(annot),as.integer(n),as.integer(K),as.integer(p),tau=double(n*K),G=double(n*K), PACKAGE="TAHMMAnnot")
}


ForwardR = function(Phi.vect,muHMM,Mat.trans.norm.vect,n,K)
{
  .C("Forward",as.double(Phi.vect),as.double(muHMM),as.double(Mat.trans.norm.vect),as.integer(n),as.integer(K),F=double(n*K), PACKAGE="TAHMMAnnot")

}

BackwardR = function(F,Mat.trans.norm.vect,n,K)
{
  .C("Backward", as.double(F),as.double(Mat.trans.norm.vect),as.integer(n),as.integer(K),tau=double(n*K),G=double(n*K), PACKAGE="TAHMMAnnot")
}

### Initialisation aleatoire
Initialization<-function(n,K){

   Tau = matrix(0,ncol=K,nrow=n)
   x<-sample(1:n)
   prop = trunc(n/K)

   for(k in 1:K){
   propStart = prop*(k-1)+1
   propEnd   = prop*k
       Tau[x[propStart:propEnd],(K-k)+1]=1
   }
   Tau[(propEnd:n),1]=1
   return(Tau)
}



Sort_file= function(fileIN="data_3colonnes_chr4_MoyenneDyeswap.txt",fileOUTdata="data_3colonnes_chr4_MoyenneDyeswap_ordonne.txt")

{

data = read.table(fileIN,header=TRUE)


IS1 = data[,2][order(data$ID)];
IS2 = data[,3][order(data$ID)];
if(dim(data)[2]==4)
{
  annot = data[,4][order(data$ID)];
  ID = sort(data$ID);

  S = data.frame(ID,IS1,IS2,annot);
}
else
{
   ID = sort(data$ID);
 
   S = data.frame(ID,IS1,IS2);
}
write.table(S,file=fileOUTdata,sep="\t",row.names=FALSE);

}

