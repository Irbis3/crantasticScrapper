multiChIPmix<-function(files=c("data1.txt","data2.txt"),init.by.PCA=TRUE,alpha=0.01,proba=0.5,eps=1e-06,fileOUT="multiChIPmix-results.txt",fileOUTgraph="multiChIPmix-results.pdf")
  {

     # data reading
   if (is.character(files)) {
        data <-read.table(files[1],header=TRUE)
	stopifnot(c("ID","INPUT","IP")%in% names(data))
	nb.samples<-length(files)
    }
    else {
	files = as.data.frame(files)
	stopifnot(c("ID","INPUT","IP")%in% names(files))
        data <- data.frame(ID=files$ID,INPUT=files$INPUT,IP=files$IP)
	nb.samples<-length(files)/3
    }
    intercept<-0*numeric(nb.samples)
    slope<-0*numeric(nb.samples)

    if (init.by.PCA)
      {
        M = cbind(data$INPUT,data$IP)
        elempropres = eigen(cov(M))
        slope[1] = elempropres$vectors[1,1]/elempropres$vectors[2,1]
        intercept[1] = mean(data$IP) - slope[1] *mean(data$INPUT)
      }
    names(data)<-paste(names(data),c("",".1",".1"),sep="")
    for (f in 1:nb.samples)
      {
  	 if (is.character(files)) {
         tmp <-read.table(files[f],header=TRUE)
   	 }
   	 else {
		tmp <-  files[,(f*3-2):(f*3)]
		names(tmp)=c("ID","INPUT","IP")
   	 }
        if (init.by.PCA)
          {
            M = cbind(tmp$INPUT,tmp$IP);
            elempropres = eigen(cov(M));
            slope[f] = elempropres$vectors[1,1]/elempropres$vectors[2,1];
            intercept[f] = mean(tmp$IP) - slope[f]*mean(tmp$INPUT);
          }
        names(tmp)<-paste(names(tmp),c("",".","."),c("",f,f),sep="")
	if (f != 1) {
          data<-merge(data,tmp,by="ID",sort=FALSE,all=TRUE)
 	}
      }
    data1<-data[order(data[,1]),]
    data<-na.omit(data1)
    n<-nrow(data)
    INPUT<-data[,grep("INPUT",names(data))]    
    IP<-data[,grep("IP",names(data))]
    IP=as.matrix(IP)
    INPUT = as.matrix(INPUT)

    #linear regression with 1 component in the mixture

    out1<-matrix(0,ncol=4,nrow=nb.samples) # per replicat : intercept, slope, variance, bic
    for (b in 1:nb.samples)
      {
        out1[b,1:2]<-lm(IP[,b]~INPUT[,b])$coefficients
        out1[b,3]<-sqrt(sum((IP[,b]-out1[b,1]-out1[b,2]*INPUT[, b])^2)/n)
        out1[b,4]<-(-2)*sum(log(dnorm(IP[,b],mean=out1[b,1]+out1[b,2]*INPUT[, b],sd=out1[b,3])))+log(n)*3     
       }
    bic1<-sum(out1[,4])
    
    #linear regression with 2 components in the mixture
    
    out2<-.EMmultiChIPmix(data,nb.samples,proba,intercept,slope,sd=rep(1,nb.samples),stop.crit=eps,max.iter=1000)
    ## analysis per replicat
    #intersection coordinates of the 2 lines (eq. (a0-a1)/(b1-b0))
    abs = (out2$a[1,]-out2$a[2,])/(out2$slope[2,]-out2$slope[1,])
    swap<-0
    INPUTindex<-matrix(0,nrow=n,ncol=nb.samples)
    for (b in 1:nb.samples)
      {
        probe.number.larger.abs = sum(INPUT[,b]>abs[b]);
        probe.number.smaller.abs = n - probe.number.larger.abs;
        if (probe.number.larger.abs > probe.number.smaller.abs )
          {
            y1 = out2$a[2,b] + out2$slope[2,b]*(abs[b]+1)
            y0 = out2$a[1,b] + out2$slope[1,b]*(abs[b]+1)
            INPUTindex[which(INPUT[,b]>abs[b]),b]<-1

          } else {
            y1 = out2$a[2,b] + out2$slope[2,b]*(abs[b]-1);
            y0 = out2$a[1,b] + out2$slope[1,b]*(abs[b]-1);
            INPUTindex[which(INPUT[,b]<abs[b]),b]<-1
          }
        if (y0 > y1)
          {
            swap<-swap+1
            out2$a[,b]<-out2$a[2:1,b] 
            out2$slope[,b]<-out2$slope[2:1,b] 
          }
      }
    if (swap!=0 && swap!=nb.samples)
      {
        cat("The two groups are swapped for one replicate  \n")
        cat("and not for all of them. The program should be stopped \n")
        cat("You should change the init.by.PCA parameter \n")
        break
      }
    if (swap==nb.samples)
      out2$proba.pi<-1-out2$proba.pi
  
    Mu0<-matrix(out2$a[1,],byrow=TRUE,ncol=nb.samples,nrow=n)+INPUT*matrix(out2$slope[1,],byrow=TRUE,ncol=nb.samples,nrow=n)
    Mu1<-matrix(out2$a[2,],byrow=TRUE,ncol=nb.samples,nrow=n)+INPUT*matrix(out2$slope[2,],byrow=TRUE,ncol=nb.samples,nrow=n)
    Phi0<-1
    Phi1<-1
     for (b in 1:nb.samples)
      {
        Phi0<-Phi0*dnorm(IP[,b],mean=as.matrix(Mu0[,b]),sd=out2$standard.error[b])
        Phi1<-Phi1*dnorm(IP[,b],mean=as.matrix(Mu1[,b]),sd=out2$standard.error[b])
      }
    # calcul du log(tau)
    cste.norm.log.tau<-log(Phi0*(1-out2$proba.pi)+Phi1*out2$proba.pi)
    log.tau<-log(Phi1*out2$proba.pi)-cste.norm.log.tau
    tau=exp(log.tau)
    log.one.minus.tau<-log(Phi0*(1-out2$proba.pi))-cste.norm.log.tau
    # calcul du critere BIC
    bic2<-(-2)*sum(cste.norm.log.tau)+log(n)*(5*nb.samples+1)
        
   #Selected model with one population     

    if(bic2 > bic1)
      {
        if(!is.null(fileOUTgraph))
          {
            pdf(file=fileOUTgraph)
            for (b in 1:nb.samples)
              {
                plot(INPUT[,b],IP[,b],pch='.',xlab="INPUT",ylab="IP",main=paste("Replicat",b,sep="."))
                x<-seq(min(INPUT[,b]),max(INPUT[,b]),length=100)
                lines(x,y=out1[b,1] + out1[b,2]*x,col="red",lwd=2)
              }
            dev.off()
          }

        cat("Only one population \n")
        parmat = out1[,1:3]
        if (nb.samples > 1)
        {
		dimnames(parmat)=list(paste("Replicat",1:nb.samples,sep="."),c("intercept","slope","std.error"))
	}
	else
	{
		names(parmat)=c("intercept","slope","std.error")
	}
        print(parmat)
        cat("\n")
	return(out1[,1:3])
      }

   #Selected model with two populations     
    
    if(bic1>bic2)
      {
         variance<-apply((Mu1-Mu0)^2/matrix(out2$standard.error^2,nrow=n,ncol=nb.samples,byrow=TRUE),1,sum)
         moy<-apply(Mu0*(Mu1-Mu0)/matrix(out2$standard.error^2,nrow=n,ncol=nb.samples,byrow=TRUE),1,sum)
         # estimation of the error risk (pvalue)
         delta<-apply((Mu1^2-Mu0^2)/matrix(out2$standard.error^2,nrow=n,ncol=nb.samples,byrow=TRUE),1,sum)
         quantity<- log.tau - log.one.minus.tau + log((1-out2$proba.pi)/(out2$proba.pi))+delta/2
         posterior.proba<-1-pnorm(quantity,mean=moy,sd=sqrt(variance))
        
         #####################################
         # output of the function 
         #####################################
         
        cat("\n")
        cat("-------------------------------- \n")
        cat("Result of multiChIPmix: \n")
        cat("-------------------------------- \n")
   	if (is.character(files)) {
        cat("files under study: \n")
        for (b in 1:nb.samples)
          print(files[b])
        cat("-------------------------------- \n")
	}
        cat(" Two populations are found \n\n")
        cat("Estimated parameters: \n\n")
        cat("Intercept \n")
        print(out2$a)
        cat("\n")
        cat("Slope \n")
        print(out2$slope)
        cat("\n")
        cat("Variance of the model per replicate \n")
        print(out2$standard.error)
        cat("\n")
        cat("Enriched probability \n")
        print(out2$proba.pi)
        cat("\n")
       
        status=as.numeric(tau>=1-alpha)
	results<-data.frame(data,tau,status)	
	

	  if(!is.null(fileOUT))
          {
            cat("data results are saved in the file", fileOUT,"\n")
            write.table(results,file=fileOUT,row.names=FALSE,sep="\t")
          }
       
        if(!is.null(fileOUTgraph))
          {
	    couleur = c()
            couleur[status == 1] = "red" 
            couleur[status == 0] = "black" 
		
            pdf(file=fileOUTgraph)
            for (b in 1:nb.samples)
              {
                plot(INPUT[,b],IP[,b],pch='.',xlab="INPUT",ylab="IP",main=paste("Replicat",b,sep="."),col=couleur)
                x<-seq(min(INPUT[,b]),max(INPUT[,b]),length=100)
                lines(x,y=out2$a[1,b]+out2$slope[1,b]*x,col="green",lwd=2)
                lines(x,y=out2$a[2,b]+out2$slope[2,b]*x,col="red",lwd=2)
              }
            dev.off()
	    cat("graphs are saved in the file", fileOUTgraph,"\n")
          }

	res = list(out=out2,status=status)
	return(res)
        #invisible(results)
      }
  }
