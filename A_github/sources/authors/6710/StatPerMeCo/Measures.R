MSE = function(S,H){
  return(mean((S-H)^2))
}

LE = function(S,H){
  return(sum((S-H)^2))
}

MAE = function(S,H){
  return(mean(abs(S-H)))
}

Frobenius = function(S,H){
  return(sum(diag((crossprod(S-H)))))
}

Stein = function(S,H){
  N = dim(S)[1]
  P = chol2inv(chol(H))%*%S
  return(sum(diag(P))-log(det(P))-N)
}

Asymm = function(S,H,b=3){
  S_b = S
  H_b = H
  for (i in 2:(b-1)){
    S_b = S_b%*%S
    H_b = H_b%*%H
  }
  return(1/(b*(b-1))*sum(diag(S_b%*%S-H_b%*%H)) - 1/(b-1)*sum(diag(H_b%*%(S-H))))
}

Leig = function(S,H){
  return(eigen((S-H)%*%(S-H))$values[1])
}

Lelw = function(S,H){
  Hinv = chol2inv(chol(H))
  return(mean(diag(Hinv%*%S%*%Hinv))/mean(diag(Hinv))^2 - 1/mean(diag(chol2inv(chol(S)))))
}

dM1 = function(A,Ahat){
  return(sqrt(1-sum(diag(tcrossprod(Ahat,Ahat)%*%tcrossprod(A,A)))/dim(A)[2]))
}

dMA = function(A,Ahat,y){
  N = dim(y)[1]
  Num = Den1 = Den2 = c()
  y_centre = scale(y,center=T,scale=F)
  for(i in 1:N){
    Num[i] = y_centre[i,]%*%tcrossprod(Ahat,A)%*%y_centre[i,]
    Den1[i] = y_centre[i,]%*%tcrossprod(Ahat,Ahat)%*%y_centre[i,]
    Den2[i] = y_centre[i,]%*%tcrossprod(A,A)%*%y_centre[i,]
  }
  return(1-sum(Num)^2/(sum(Den1)*sum(Den2)))
}


