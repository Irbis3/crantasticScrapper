bound.standard <-
function(bounds,Ne){
      out <- vector("numeric",2)
      if(Ne==1){ out <- c(1,1)
      }else{        
          if(bounds[1]==0){ out[1] <- 1; #warning("Lower bound on cost should not be zero!");
                            vec    <- which(min(abs(bounds[2]*Ne-1:Ne))==abs(bounds[2]*Ne-1:Ne));
                            out[2] <- vec[trunc(runif(1,1,length(vec)+1))];
          }else{
          for(k in 1:2){
              if(belong(bounds[k],1:Ne/Ne)){
                out[k] <- bounds[k]*Ne
              }else{
                vec    <- which(min(abs(bounds[k]*Ne-1:Ne))==abs(bounds[k]*Ne-1:Ne));
                out[k] <- vec[trunc(runif(1,1,length(vec)+1))];
                #warning(paste("Bound ",k," is not a cost!"));
              }#fi
            }#k 
          }#fi
      }#fi
      out <- out/Ne
return(out)
}

