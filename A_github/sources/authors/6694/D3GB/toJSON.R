toJSON <- function(x){
  json <- ""
  if(length(x)<=1){
    if(is.null(x)||identical(is.na(x),TRUE)){
      json <- "null"
    }else{
      if(is.vector(x)){
	if(is.numeric(x)){
          if(length(x)==0)
            json <- "[]"
          else
	    json <- x
	}
	if(is.logical(x)){
	  if(identical(x,TRUE))
	    json <- "true"
	  else
	    json <- "false"
	}
	if(is.character(x))
	  json <- paste0('"',x,'"')
	if(is.list(x)){
	  if(length(x)==0)
	    json <- "{}"
	  else{
	    if(is.null(names(x))){
	      json <- paste0("[", toJSON(x[[1]]), "]", collapse = "")
	    }else{
	      aux <- paste0('"',names(x),'":',toJSON(x[[1]]))
	      json <- paste0("{", aux, "}", collapse = "")
	    }
          }
	}
      }
      if(is.factor(x)){
	json <- paste0('"',x,'"')
      }
      if(is.array(x)){
	if(dim(x)[1]>0)
	  aux <- toJSON(as.vector(x))
	else
	  aux <- ""
	json <- paste0("[",aux,"]", collapse = "")
      }
      if(is.data.frame(x)){
	aux <- apply(x, 1, function(x)  paste0('[', toJSON(x), ']', collapse = ""))
	aux <- paste0(aux , collapse = ",")
	json <- paste0("[", aux, "]", collapse = "")
      }
    }
  } else {
    if(is.vector(x)||is.factor(x)){
      aux <- paste0(lapply(x, toJSON), collapse = ",")
      json <- paste0("[", aux, "]", collapse = "")
    }
    if(is.array(x)){
      aux <- apply(x, 1, toJSON)
      aux <- paste0(aux, collapse = ",")
      json <- paste0("[", aux, "]", collapse = "")
    }
    if(is.data.frame(x)){      
      aux <- lapply(seq_len(dim(x)[1]), function(x,z) paste0("[", paste0(lapply(seq_along(z[x,]), function(x,y) toJSON(y[[x]]), y=z[x,]), collapse = ","), "]", collapse = ""), z=x)
      aux <- paste0(aux , collapse = ",")
      json <- paste0("[", aux, "]", collapse = "")
    }
    if(is.list(x)&&!is.data.frame(x)){
      if(is.null(names(x))){
	aux <- paste0(lapply(x, toJSON), collapse = ",")
	json <- paste0("[", aux, "]", collapse = "")
      }else{
	aux <- lapply(seq_along(x), function(x,y,n) paste0('"',n[[x]],'":',toJSON(y[[x]])),y=x,n=names(x))
	aux <- paste0(aux , collapse = ",")
	json <- paste0("{", aux, "}", collapse = "")
      }      
    }
  }
  return(json)
}
