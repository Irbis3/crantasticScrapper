  ## __________________________________________________________
  ##
  ## .ForwardR
  ##
  ## __________________________________________________________
  ##

.ForwardR = function(Phi.vect,muHMM,Mat.trans.norm.vect,n)
{
  .C("Forward",as.double(Phi.vect),as.double(muHMM),as.double(Mat.trans.norm.vect),as.integer(n),as.integer(2),F=double(n*2), PACKAGE="MultiChIPmixHMM")

}

  ## __________________________________________________________
  ##
  ## .BackwardR
  ##
  ## __________________________________________________________
  ##

.BackwardR = function(F,Mat.trans.norm.vect,n)
{
  .C("Backward", as.double(F),as.double(Mat.trans.norm.vect),as.integer(n),as.integer(2),tau=double(n*2),G=double(n*2), PACKAGE="MultiChIPmixHMM")
}


  ## __________________________________________________________
  ##
  ## .EMmultiChIPmix
  ##
  ## __________________________________________________________
  ##

.EMmultiChIPmix<-function(data,nb.samples,proba,intercept,slope,sd,stop.crit,max.iter=1000)
 {
   variance<-numeric(nb.samples)
   eps.tau<-1e-16
   slope0<-slope*95/100
   slope1<-slope*105/100
   INPUT<-data[,grep("INPUT",names(data))]    
   IP<-data[,grep("IP",names(data))]
   IP=as.matrix(IP)
   INPUT = as.matrix(INPUT)
   intercept0<-apply(IP,2,mean,na.rm=TRUE)-slope0*apply(INPUT,2,mean,na.rm=TRUE)
   intercept1<-apply(IP,2,mean,na.rm=TRUE)-slope1*apply(INPUT,2,mean,na.rm=TRUE)
   difference<-2*stop.crit
   iter<-0
   while( (difference > stop.crit) & (iter < max.iter) )
     {
       iter<-iter+1
      old.parameters<-c(pi=proba,intercept0=intercept0,intercept1=intercept1,slope0=slope0,slope1=slope1,standard.error=sd)

      tau0<-rep((1-proba),nrow(data))
      tau1<-rep(proba,nrow(data))
      for (b in 1:nb.samples)
        {
          mean.param<-intercept0[b]+slope0[b]*INPUT[,b]
          tau0<-tau0*dnorm(IP[,b],mean=mean.param,sd=sd[b])
          mean.param<-intercept1[b]+slope1[b]*INPUT[,b]
          tau1<-tau1*dnorm(IP[,b],mean=mean.param,sd=sd[b])
        }
      tau<-tau1/(tau0+tau1)
      
    # smoothing
      tau<-apply(cbind(tau,eps.tau),1,max)
      tau<-apply(cbind(tau,1-eps.tau),1,min)
      
      
    # M step
      proba<-mean(tau)
      wt.INPUT.0<-apply((1-tau)*INPUT/sum(1-tau),2,sum)
      wt.IP.0<-apply((1-tau)*IP/sum(1-tau),2,sum)
      wt.INPUT.1<-apply(tau*INPUT/sum(tau),2,sum)
      wt.IP.1<-apply(tau*IP/sum(tau),2,sum)

      centered.INPUT.0<-INPUT-matrix(wt.INPUT.0,nrow=nrow(data),ncol=nb.samples,byrow=TRUE)
      centered.INPUT.1<-INPUT-matrix(wt.INPUT.1,nrow=nrow(data),ncol=nb.samples,byrow=TRUE)

      for (b in 1:nb.samples)
        {
          deno<-(1-tau)%*%(centered.INPUT.0[,b]^2)
          slope0[b]<-(IP[,b]*(1-tau))%*%centered.INPUT.0[,b]/deno
         
          deno<-tau%*%(centered.INPUT.1[,b]^2)
          slope1[b]<-(IP[,b]*tau)%*%centered.INPUT.1[,b]/deno
          intercept0[b]<-wt.IP.0[b]-slope0[b]*wt.INPUT.0[b]
          intercept1[b]<-wt.IP.1[b]-slope1[b]*wt.INPUT.1[b]

          error0<-(1-tau)%*%((IP[,b]-intercept0[b]-slope0[b]*INPUT[,b])^2)
          error1<-tau%*%((IP[,b]-intercept1[b]-slope1[b]*INPUT[,b])^2)
          sd[b]<-sqrt((error0+error1)/nrow(data))
        }
      
      relative.diff<-(old.parameters-c(proba,intercept0,intercept1,slope0,slope1,sd))/old.parameters
      difference<-max(abs(relative.diff))
      cat("iteration number : ",iter,"\n")
      cat("parameters : ",c(proba,intercept0,intercept1,slope0,slope1,sd),"\n")
      cat("max of relative difference : ", difference,"\n")
    }
    a<-rbind(intercept0,intercept1)
    slope<-rbind(slope0,slope1)
    colnames(a)<-paste("replicat",1:nb.samples,sep=".")
    colnames(slope)<-paste("replicat",1:nb.samples,sep=".")
    standard.error<-sd
    names(standard.error)<-paste("replicat",1:nb.samples,sep=".")
    out<-list(proba.pi=proba,a=a,slope=slope,standard.error=sd)
 }


  ## __________________________________________________________
  ##
  ## .EMmultiChIPmixHMM
  ##
  ## __________________________________________________________
  ##

.EMmultiChIPmixHMM<-function(data,nb.samples,proba,intercept,slope,sd,n,stop.crit,max.iter=1000)
  {
    variance<-numeric(nb.samples)
    eps.tau<-1e-16
    slope0<-slope*95/100
    slope1<-slope*105/100
    INPUT<-data[,grep("INPUT",names(data))]    
    IP<-data[,grep("IP",names(data))]
    IP=as.matrix(IP)
    INPUT = as.matrix(INPUT)
    intercept0<-apply(IP,2,mean,na.rm=TRUE)-slope0*apply(INPUT,2,mean,na.rm=TRUE)
    intercept1<-apply(IP,2,mean,na.rm=TRUE)-slope1*apply(INPUT,2,mean,na.rm=TRUE)
    difference<-2*stop.crit
    iter<-0
#initialisation of  tau
#    tau0<-rep((1-proba),nrow(data))
#    tau1<-rep(proba,nrow(data))
#    tau = cbind(tau0,tau1)
#initialisation of transition matrix
    Mat.trans.norm = matrix(c(proba,1-proba,1-proba,proba),nrow=2,ncol=2,byrow=TRUE)
  
#initialisation of stationnary distribution
    val.propre = round(eigen(t(Mat.trans.norm))$values,3)
    pos = which(val.propre == 1.000)
    muHMM = eigen(t(Mat.trans.norm))$vectors[,pos]
    muHMM = muHMM / sum(muHMM)
    muHMM = as.numeric(muHMM)
    
        
    while( (difference > stop.crit) & (iter < max.iter) )
    {
      iter<-iter+1
      old.parameters<-c(pi=proba,intercept0=intercept0,intercept1=intercept1,slope0=slope0,slope1=slope1,standard.error=sd)

 #Calcul de Phi
      for (b in 1:nb.samples)
        {
          mean.param1<-intercept0[b]+slope0[b]*INPUT[,b]
          mean.param2<-intercept1[b]+slope1[b]*INPUT[,b]
          
          Phi = matrix(rep(1,n),nrow=n,ncol=2)
          Phi[,1] = Phi[,1]*dnorm(IP[,b],mean=mean.param1,sd=sd[b])
          Phi[,2] = Phi[,2]*dnorm(IP[,b],mean=mean.param2,sd=sd[b])
        }
     
### Forward-Backward algorithm ###
 
      Phi.vect = c(Phi[,1],Phi[,2])
  
      Mat.trans.norm.vect = c(Mat.trans.norm[,1],Mat.trans.norm[,2])
      resF = .ForwardR(Phi.vect,muHMM,Mat.trans.norm.vect,n)
      resB = .BackwardR(resF$F,Mat.trans.norm.vect,n)

      tau = matrix(resB$tau,nrow=n,ncol=2,byrow=FALSE) 
      F = matrix(c(resF$F),nrow=n,ncol=2)
      G = matrix(c(resB$G),nrow=n,ncol=2)
         # smoothing
      tau[,2]<-apply(cbind(tau[,2],eps.tau),1,max)
      tau[,2]<-apply(cbind(tau[,2],1-eps.tau),1,min)
      tau[,1] = 1-tau[,2]
    ##################################### M step
#Update the transition matrix
      Mat.trans = Mat.trans.norm * (t(F[-n,])%*% (tau[-1,] / G[-1,]))
      Mat.trans.norm = Mat.trans/rowSums(Mat.trans)
       
#Update the stationnary distribution 
      val.propre = round(eigen(t(Mat.trans.norm))$values,3)
      pos = which(val.propre == 1.000)
      muHMM = eigen(t(Mat.trans.norm))$vectors[,pos]
      muHMM = muHMM / sum(muHMM)
      muHMM = as.numeric(muHMM)
      
     
      tau<-tau[,2]
     
      proba<-mean(tau)
  
      wt.INPUT.0<-apply((1-tau)*INPUT/sum(1-tau),2,sum)
      wt.IP.0<-apply((1-tau)*IP/sum(1-tau),2,sum)
      wt.INPUT.1<-apply(tau*INPUT/sum(tau),2,sum)
      wt.IP.1<-apply(tau*IP/sum(tau),2,sum)

      centered.INPUT.0<-INPUT-matrix(wt.INPUT.0,nrow=nrow(data),ncol=nb.samples,byrow=TRUE)
      centered.INPUT.1<-INPUT-matrix(wt.INPUT.1,nrow=nrow(data),ncol=nb.samples,byrow=TRUE)
      for (b in 1:nb.samples)
        {
          deno<-(1-tau)%*%(centered.INPUT.0[,b]^2)
          slope0[b]<-(IP[,b]*(1-tau))%*%centered.INPUT.0[,b]/deno
         
          deno<-tau%*%(centered.INPUT.1[,b]^2)
          slope1[b]<-(IP[,b]*tau)%*%centered.INPUT.1[,b]/deno
          intercept0[b]<-wt.IP.0[b]-slope0[b]*wt.INPUT.0[b]
          intercept1[b]<-wt.IP.1[b]-slope1[b]*wt.INPUT.1[b]

          error0<-(1-tau)%*%((IP[,b]-intercept0[b]-slope0[b]*INPUT[,b])^2)
          error1<-tau%*%((IP[,b]-intercept1[b]-slope1[b]*INPUT[,b])^2)
          sd[b]<-sqrt((error0+error1)/nrow(data))
        }
     
     
      
      relative.diff<-(old.parameters-c(proba,intercept0,intercept1,slope0,slope1,sd))/old.parameters
      difference<-max(abs(relative.diff))
     
      cat("iteration number : ",iter,"\n")
      cat("parameters : ",c(proba,intercept0,intercept1,slope0,slope1,sd),"\n")
      cat("max of relative difference : ", difference,"\n")
      
    }
    a<-rbind(intercept0,intercept1)
    slope<-rbind(slope0,slope1)
    colnames(a)<-paste("replicat",1:nb.samples,sep=".")
    colnames(slope)<-paste("replicat",1:nb.samples,sep=".")
    standard.error<-sd
    names(standard.error)<-paste("replicat",1:nb.samples,sep=".")

    out<-list(proba.pi=proba,a=a,slope=slope,standard.error=sd,tau=tau,Mat.trans=Mat.trans.norm)

}


