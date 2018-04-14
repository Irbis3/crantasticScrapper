Mixture_Annot = function(color=c("navajowhite","grey","black","green","red"),fileIN="MoyDye_CHR01.txt",var1="IS1",var2="IS2",var3="annot",K=4, p=3, eps=10e-6, file.RData="Param_CHR01.RData",file.graph="Graph_CHR01.png",file.flagdb="Fichier_flagdb.txt",file.OUT="Output_CHR01.txt",file.INIT="Graph_init_CHR01.png",file.param="Fichier_Param_CHR01.txt",threshold=FALSE,s=0.7,random.init=FALSE,a.inf=60,a.sup=75,b.inf=5,b.sup=20,PtInit=8,theta=0.4,int.max=8,max.iter=1000,header=TRUE,sep="\t", ...)
{

 if (length(color) < K)
 {
  cat("ERROR: You should add component in the argument color to have one color per group \n")
  stop()
 }

library(mnormt)
path = attr(as.environment(match("package:TAHMMAnnot",search())),"path")

data = read.table(fileIN,header=header,sep=sep, ...);
ID = data$ID;
n=length(ID)
Int1 = data[,match(var1, names(data))]
Int2 = data[,match(var2, names(data))]
annot = data[,match(var3, names(data))]

##Test si il y a des intensites a 0.0
if(min(Int1)=="-Inf" | min(Int2)=="-Inf")
{
   print("Error : The following probes have Int1 = 0.0")
   print(as.vector(ID[which(Int1=="-Inf")]))
   print("Error : The following probes have Int2=0.0")
   print(as.vector(ID[which(Int2=="-Inf")]))
   stop("Error : There are null intensities")
   
}

x=matrix(c(Int1,Int2),nrow=n,ncol=2);

#Trouver le chromosome
if(length(grep("FS",as.character(ID[1]))==1)==1) sp<-"FS" else sp<-"RS"
nom.tmp<-strsplit(as.character(ID[1]),split=sp)[[1]][1]

diffannot = diff(annot)

xbis = cbind(x,annot)

###initialisation des variables###

W=list()
D=list()
B=list()
Lambda=list()
mu=list()
sigma=list()

Pi=matrix(0,ncol=p,nrow=K)
ti = matrix(0,nrow=n,ncol=K)
cpt1 =0
Q=c()

if(K!=4 & K!=3)
{
  print("Random initialization because K is different from 4")
  cat("\n")
  random.init=TRUE
}

if(random.init==FALSE & K==4)
{

#initialisation precise pour K=4

init.a<-seq(min(x[,1]),min(x[,1])+4,0.5)
tmp<-apply(as.matrix(init.a),1,FUN=function(y) 100*length(which(x[,1]<y & x[,2]<y))/n)
if( length(which(tmp<a.sup & tmp>a.inf))==0)
{
  init.a=init.a[which.min(abs(tmp-a.sup))]
}else {
init.a<-min(init.a[which(tmp<a.sup & tmp>a.inf)])
}

ind1 = which(x[,1]<init.a & x[,2]<init.a)

init.b<-seq(0.25,1,0.25)
tmp<-apply(as.matrix(init.b),1,FUN=function(y) 100*length(which(x[,2]>=x[,1]-y & x[,2]<=x[,1]+y & (x[,1]>=init.a | x[,2]>=init.a)))/n)
if( length(which(tmp<b.sup & tmp>b.inf))==0)
{
  init.b=init.b[which.min(abs(tmp-b.inf))]
}else {
   init.b<-max(init.b[which(tmp<b.sup & tmp>b.inf)])
}

ind2 = which(x[,2]>=x[,1]-init.b & x[,2]<=x[,1]+init.b & (x[,1]>=init.a | x[,2]>=init.a))
ind3 = which(x[,2]<x[,1]-init.b & (x[,1]>=init.a | x[,2]>=init.a))
ind4 = which(x[,2]>x[,1]+init.b & (x[,1]>=init.a | x[,2]>=init.a))

 ti[ind1,1] = 1
 ti[ind2,2] = 1
 ti[ind3,3] = 1
 ti[ind4,4] = 1
}


if(random.init==FALSE & K==3)
{

  intercept<-c()
  slope<-c()

  #Droite du milieu
  intercept[2] <- 0
  slope[2] <- 1


  #Droite du haut
  intercept[1] <- (1/theta)*(-sqrt(1-theta**2)+theta)*PtInit
  slope[1] <- (1/theta)*sqrt(1-theta**2)
  #Droite du bas
  intercept[3] <- (1/sqrt(1-theta**2))*(-theta+sqrt(1-theta**2))*PtInit
  slope[3] <- (1/sqrt(1-theta**2))*theta


  x1 = (x[,2] - intercept[1]) / slope[1]
  x2 = (x[,2] - intercept[2]) / slope[2]
  x3 = (x[,2] - intercept[3]) / slope[3]

ind1 = which(x[,1]<int.max & x[,2]<int.max)
ind2 = which(x[,1] < x3 & x[,1] > x1 & (x[,1]>=int.max | x[,2]>=int.max))
ind3 = which(x[,1] > x3 & (x[,1]>=int.max | x[,2]>=int.max))
ind4 = which(x[,1] < x1 & (x[,1]>=int.max | x[,2]>=int.max))

if(length(ind3)>length(ind4))
{
  ti[ind1,1] = 1
  ti[c(ind2,ind4),2] = 1
  ti[ind3,3] = 1

} else if(length(ind4)>length(ind3))
{
  ti[ind1,1] = 1
  ti[c(ind2,ind3),2] = 1
  ti[ind4,3] = 1


}

#autre sol pour K=4, avec des droites et theta : le debut idem que pour K=3 et on met les ti classiques

# ti[ind1,1] = 1
# ti[ind2,2] = 1
# ti[ind3,3] = 1
# ti[ind4,4] = 1

}


if(random.init==TRUE)
{

## Initialisation aleatoire
 ti=Initialization(n,K)

}

### Tracer le graphe d'initialisation ###

ind=unlist(apply(ti,1,which.max))


png(file.INIT)
plot(x[,1],x[,2],pch='.',col=color[ind+1],xlab=var1,ylab=var2)
dev.off()


repeat {

cpt1 = cpt1 +1
print(cpt1)
cat("\n")

  
###Calcul des Pik ###
for(i in 1:p)
{
  Pi[,i] = colSums(ti[which(annot==i),]) / length(which(annot==i)) 
}


###Mise a jour de mu (mu=xbar)###
deno = apply(ti,2,sum)
for(k in 1:K)
{ 
 
###Calcul de mu sans la fct cov.wt###
   mu[[k]] = colSums(ti[,k]*x)/deno[k]
   mu[[k]] = t(mu[[k]])

###Calcul de Wk### 

   q = t(apply(x,1,matW,k,mu))
   z = ti[,k]*q
   W[[k]] = matrix(colSums(z),ncol=2,nrow=2)
}

###Calcul de sigma###


###Calcul de D et Dk###

Dmat = CalculDirect(W)
D = lapply(W,FUN=function(x) eigen(x)$vectors)
D[[1]] = D[[2]] = Dmat


###Calcul de B###

B = mapply(ProdMat,D,W,SIMPLIFY = FALSE)


###Calcul de Lambdak###

nk = colSums(ti);
lambda2 = sum(unlist(lapply(B,Acces,i=2,j=2)))
for(i in 1:K)
{
   Lambda[[i]] = matrix(c(B[[i]][1,1]/nk[[i]],0,0,lambda2/n),nrow=2,ncol=2) 
}


###Calcul de sigma (estimateur)###

sigma = mapply(ProdMat2,D,Lambda,SIMPLIFY = FALSE)



###criteres de convergence###
if (cpt1 != 1){
chnge.Pi <- max(abs((unlist(Pi) - unlist(Pi.old))/unlist(Pi.old)))
chnge.mu <- max(abs((unlist(mu) - unlist(mu.old))/unlist(mu.old)))
chnge.sigma <- max(abs((unlist(sigma) - unlist(sigma.old))/unlist(sigma.old)))

print( max(chnge.Pi,chnge.mu,chnge.sigma))
 if ( (max(chnge.Pi,chnge.mu,chnge.sigma) < eps) || (cpt1 == max.iter))
 {
   break 
 }
}


###On garde les paramètres de l'étape d'avant###
Pi.old = Pi
mu.old = mu
sigma.old = sigma


###Calcul des tik###


ti = apply(xbis,1,tauannot,sigma,mu,Pi)
ti = t(ti) #matrice n*K
 
#smoothing des tau
ti = apply(ti,2,pmax,10^-6)
ti=ti/rowSums(ti)


}#end repeat EM


#Definition des groupes
if(K>=2)
{
#le groupe 1 est tjs le bruit
if (mu[[2]][,1] < mu[[1]][,1])
{
  tmp = mu[[2]]
  mu[[2]] = mu[[1]]
  mu[[1]] = tmp

  tmp = Lambda[[2]]
  Lambda[[2]] = Lambda[[1]]
  Lambda[[1]] = tmp

  tmp = D[[2]]
  D[[2]] = D[[1]]
  D[[1]] = tmp

  tmp = B[[2]]
  B[[2]] = B[[1]]
  B[[1]] = tmp

  tmp = W[[2]]
  W[[2]] = W[[1]]
  W[[1]] = tmp

  tmp = sigma[[2]]
  sigma[[2]] = sigma[[1]]
  sigma[[1]] = tmp
 
  tmp = ti[,2] 
  ti[,2] = ti[,1]
  ti[,1] = tmp


}
}
if(K>=4)
{
#le groupe 3 est tjs sous la bissectrice
if (mu[[3]][,1] < mu[[3]][,2])
{
  tmp = mu[[4]]
  mu[[4]] = mu[[3]]
  mu[[3]] = tmp

  tmp = Lambda[[4]]
  Lambda[[4]] = Lambda[[3]]
  Lambda[[3]] = tmp

  tmp = D[[4]]
  D[[4]] = D[[3]]
  D[[3]] = tmp

  tmp = B[[4]]
  B[[4]] = B[[3]]
  B[[3]] = tmp

  tmp = W[[4]]
  W[[4]] = W[[3]]
  W[[3]] = tmp

  tmp = sigma[[4]]
  sigma[[4]] = sigma[[3]]
  sigma[[3]] = tmp
 
  tmp = ti[,4] 
  ti[,4] = ti[,3]
  ti[,3] = tmp

}
}


#Creation du fichier pour FLAGdb
S = data.frame(ID,ti,x[,1],x[,2])
if(nom.tmp=="CHR01")
{
  write.table(S,file.flagdb,sep="\t",row.names=FALSE,col.names=c("ID",paste("p",1:K,sep=""),"Int1","Int2"))
}else {
  write.table(S,file.flagdb,append=TRUE,sep="\t",row.names=FALSE,col.names=FALSE)
}

#Cration du fichier texte avec les parametres
sink(file.param)
cat("Pi\n")
print(Pi)
cat("\n")
cat("D\n")
print(D)
cat("\n")
cat("Lambda\n")
print(Lambda)
cat("\n")
cat("mu\n")
print(mu)
cat("\n")
sink()


if(threshold==FALSE)
{

  ### Classement par la règle du MAP et graphe de sortie ###
  ind=unlist(apply(ti,1,which.max))


  png(file.graph)
  plot(x[,1],x[,2],pch='.',col=color[ind+1],xlab=var1,ylab=var2)
  dev.off()
 
  ratio = as.vector(x[,2]) - as.vector(x[,1])
  S = data.frame(ID,x[,1],x[,2],ratio,ti,status=ind)
  write.table(S,file.OUT,sep="\t",row.names=FALSE,col.names=c("ID","Int1","Int2","ratio",paste("p",1:K,sep=""),"status"))
}else{
 
  ind=unlist(apply(ti,1,FUN=function(x,threshold) sum(x>threshold)*which.max(x),s))


 
png(file.graph)
plot(x[,1],x[,2],pch='.',col=color[ind+1],xlab=var1,ylab=var2)
dev.off()

S = data.frame(ID,x[,1],x[,2],ratio=as.vector(x[,2]) - as.vector(x[,1]),ti,status=ind)
write.table(S,file.OUT,sep="\t",row.names=FALSE,col.names=c("ID","Int1","Int2","ratio",paste("p",1:K,sep=""),"status"))
}

save(ti,Pi,mu,sigma,W,Q,D,Lambda,B,file=file.RData)
invisible(list(ti=ti,Pi=Pi,mu=mu,sigma=sigma,D=D,Lambda=Lambda,W=W,B=B,ind=ind))


}#end function




