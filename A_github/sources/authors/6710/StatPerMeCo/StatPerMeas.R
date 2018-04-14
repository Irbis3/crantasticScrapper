StatPerMeas = function(S,H,measure="ALL", b=3){
  if(nrow(S)!=ncol(S)){
    stop("S is not square")
  }
  if(nrow(H)!=ncol(H)){
    stop("H is not square")
  }
  if(nrow(H)!=nrow(S)){
    stop("Matrices do not have the same dimension")
  }
  if(!isSymmetric(S)){
    stop("S is not symmetric")
  }
  if(!isSymmetric(H)){
    stop("H is not symmetric")
  }
  if(sum(eigen(H)$values<=0)){
    stop("H is not positive definite")
  }
  if(sum(eigen(S)$values<=0)){
    stop("S is not positive definite")
  }

  if(sum(c("Le", "MSE", "MAE", "Lf", "Ls", "Asymm", "Leig", "Lelw", "ALL")%in%measure)<1){
    stop("Incorret inputs in 'measure' option")
  }else{
    if("ALL"%in%measure){
      N = nrow(H)
      measures = data.frame(matrix(c(LE(S,H),MSE(S,H),MAE(S,H),Frobenius(S,H),Stein(S,H),Asymm(S,H,b=b),Leig(S,H), Lelw(S,H)),ncol=1))
      rownames(measures)=c("Euclidean distance", "Mean Square Error","Mean Absolute Error","Frobenius distance","Stein loss function","Asymmetric loss function","Eigenvalue loss function",
                           "Elw Loss function")
      colnames(measures)=c("Value")
      message("Statistical Performance Measures:")
      return(measures)
    } else{
      measures = matrix(NA,ncol=1,nrow=8)
      if ("Le"%in%measure)    measures[1,1] = LE(S,H)
      if ("MSE"%in%measure)   measures[2,1] = MSE(S,H)
      if ("MAE"%in%measure)   measures[3,1] = MAE(S,H)
      if ("Lf"%in%measure)    measures[4,1] = Frobenius(S,H)
      if ("Ls"%in%measure)    measures[5,1] = Stein(S,H)
      if ("Asymm"%in%measure) measures[6,1] = Asymm(S,H,b=b)
      if ("Leig"%in%measure)  measures[7,1] = Leig(S,H)
      if ("Lelw"%in%measure)  measures[8,1] = Lelw(S,H)

      measures = data.frame(measures)
      rownames(measures)=c("Euclidean distance", "Mean Square Error","Mean Absolute Error","Frobenius distance","Stein loss function","Asymmetric loss function","Eigenvalue loss function",
                           "Elw Loss function")
      colnames(measures)=c("Value")
      measures = na.exclude(measures)
      if(dim(measures)[1]!=length(measure)){
        message("Some options are invalid. Correct options:")
        cat(c("Le", "MSE", "MAE", "Lf", "Ls", "Asymm", "Leig", "Lelw", "ALL")[c("Le", "MSE", "MAE", "Lf", "Ls", "Asymm", "Leig", "Lelw", "ALL")%in%measure])
      }
      message("Statistical Performance Measures:")
      return(measures)
    }
  }
  }


