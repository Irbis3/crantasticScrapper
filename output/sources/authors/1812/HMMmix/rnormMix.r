  rnormMix<- function(n,Mat.transition,init,mean,varcov,eta)
  {
  D = length(init)
  Density = matrix(0,ncol=(D),nrow=n)
  Markov.chain = rmultinom(1,1,init)
  class = which(Markov.chain[,1]==1)
  group.mix = rmultinom(1,1,eta[[class]])
  data = rmnorm(1,mean[[class]][,which(group.mix==1)],varcov[[class]][[which(group.mix==1)]])

  for(j in 1:D)
  {
  k1 = dim(mean[[j]])[2]
    for( k in 1:k1)
    {
    Density[1,j] = Density[1,j] + eta[[j]][k]*dmnorm(data,mean[[j]][,k],varcov[[j]][[k]])
    }
  }

  for(i in 2:n)
  {
  Group.neigh = class[i-1]
  Z = rmultinom(1,1,Mat.transition[Group.neigh,])
  tmp = which(Z[,1]==1)
  group.mix = rmultinom(1,1,eta[[tmp]])
  data.tmp = rmnorm(1,mean[[tmp]][,which(group.mix==1)],varcov[[tmp]][[which(group.mix==1)]])
  data = rbind(data,data.tmp)

    for(j in 1:D)
    {
    k1 = dim(mean[[j]])[2]
      for( k in 1:k1)
      {
      Density[i,j] = Density[i,j] + eta[[j]][k]*dmnorm(data.tmp,mean[[j]][,k],varcov[[j]][[k]])
      }

    }
  class = c(class,which(Z[,1]==1))
  }
  Density = apply(Density,2,pmax,0.0000000000001)
  EM = .E.step.HMM(Density,init,Mat.transition)
  Tau = EM$Tau

  return(list(data=data,class=class,Tau=Tau))
  }