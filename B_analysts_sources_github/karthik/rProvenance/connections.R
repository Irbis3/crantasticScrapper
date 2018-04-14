traceConnectionFun =
  #
  # A slightly different approach to capturing information
  # about files or connections generally.
  # We take the function and some code to capture the information
  # We start the function call by computing information about the connection object
  # e.g. 
function(fun, conName = names(formals(fun))[1], where = globalenv())
{
   name = deparse(substitute(fun))

   b = body(fun)
   b[(1:length(b)) + 1L] = b

   if(is.character(conName) || is.name(conName))
     e = substitute(.conInitState <- getConnectionState(con), list(con = as.name(conName)))
   else
     e = conName
   
   b[[2]] = e
   body(fun) = b
   q = substitute(addProvenance(x, sys.calls(), sys.frames(), sys.nframe(), files = con, .conInitialState = .conInitState), list(con = conName))
   provenanceTrace(fun, q)
 }


setGeneric("getConnectionState",
            function(x, ...)
              standardGeneric("getConnectionState"))

setMethod("getConnectionState", "character",
           function(x, ...)
             file.info(x))

setMethod("getConnectionState", "connection",
           function(x, ...) {
             info = summary(x)
             info$seek = seek(x)

             if(is(x, "file"))
               info$info = file.info(x)
             
             info
           })
           
  
