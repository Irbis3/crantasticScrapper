########################################
# belong function for character.
belong  <- function(element,
                    set
                    ){
  n <- length(set)
  logical <- FALSE
  if(length(set)==0){logical <- FALSE}
  else{
    for (i in 1:n){ if(element==set[i]){logical <- TRUE }}
  }
return(logical)
} # End belong
