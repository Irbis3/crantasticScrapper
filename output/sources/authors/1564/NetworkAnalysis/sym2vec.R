#
# Faster C++ Wrapper.
# 
#############################
sym2vec <- function(A){
    if(class(A[1,1])=="integer"){
       Avec <- .Call("sym2vec_integer",PACKAGE="NetworkAnalysis",as.matrix(A))      
    }else{
       Avec <- .Call("sym2vec_numeric",PACKAGE="NetworkAnalysis",as.matrix(A))      
    }
return(Avec)  
}# sym2vec wrapper. 
