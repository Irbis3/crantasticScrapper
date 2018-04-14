  plotHMMmix<-function(x)
  {

  stopifnot(ncol(x$data)==2)
  par(mfrow=c(2,2),font.sub=2)
  data=x$data
  
  ## Plot start
  t = seq(0,2*pi,0.1)
  alpha = 0.3
  varcov = x$estimation$parameters$varcov
  mean  = x$estimation$parameters$mean
  K = dim(mean)[2]
  diago  = eigen(varcov)
  theta  = atan(diago$vectors[2,1]/diago$vectors[1,1])
  Lambda = diago$values
  r = sqrt(-2*log(alpha))
  name = paste("Initial fit with ",K," components",sep="")
  plot.default(data,xlab="X",ylab="Y",main=name,lwd=1.5,cex=1.5)

  for(l in 1:K)
  {
  x2 = sqrt(Lambda[1])*cos(t)*r
  y2 = sqrt(Lambda[2])*sin(t)*r
  x2bis = (x2*cos(theta)-y2*sin(theta)) + mean[1,l]
  y2bis = (x2*sin(theta)+y2*cos(theta)) + mean[2,l]
  lines.default(x2bis,y2bis,xlim=c(0,25),ylim=c(0,25),col=l+1,lwd=2.5)
  }



  ## Plot criterion
  ind.na = which(is.na(as.vector(x$crit.mat)))
  plot.default(x$crit.mat[,1],type="b",cex=2,ylim=c(min(as.vector(x$crit.mat)[-ind.na]),max(as.vector(x$crit.mat)[-ind.na]))
  ,xlab="Number of clusters",ylab="Criteria",main="BIC and ICLs criterion")
  lines.default(x$crit.mat[,2],type="b",cex=2,pch=2)
  points.default(which.max(x$crit.mat[,1]),x$crit.mat[which.max(x$crit.mat[,1]),1],col="red",lwd=1.5,cex=2)
  points.default(which.max(x$crit.mat[,2]),x$crit.mat[which.max(x$crit.mat[,2]),2],col="red",lwd=1.5,cex=2,pch=2)
  legend("bottomleft", c("BIC","ICLs"),pch=c(1,2))
       
  ## Plot classification with D cluster
  classD = x$DstatesHMM$class
  name = paste("Classification with ",max(classD)," clusters (D)",sep="")
  plot.default(data,xlab="X",ylab="Y",col=classD,cex=1.5,lwd=1.5,main=name)
  
  ## Plot classification with MaxICLS cluster
  classMaxICL = x$MaxICLstatesHMM$class
  name = paste("Classification with ",max(classMaxICL)," clusters (Maximum of ICLs)",sep="")
  plot.default(data,xlab="X",ylab="Y",col=classMaxICL,cex=1.5,lwd=1.5,main=name)


  }